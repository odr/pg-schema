{-# LANGUAGE OverloadedRecordDot #-}
module PgSchema.DML.InsertJSON
  ( InsertMode(..)
  , insertJSON, insertJSON_, upsertJSON, upsertJSON_
  , updateJSON, updateJSON_
  , insertJSONText, insertJSONText_, upsertJSONText, upsertJSONText_
  , updateJSONText, updateJSONText_ ) where

import Control.Monad
import Control.Monad.RWS
import Data.Aeson as A
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Bifunctor
import Data.Foldable as F
import Data.Function
import Data.Functor
import Data.List as L
import Data.Map as M hiding (mapMaybe)
import Data.Maybe
import Data.Text as T hiding (any)
import Data.Traversable
import PgSchema.Ann
import PgSchema.DML.Insert.Types
import PgSchema.DML.KeyedWrite (mandatoryDbNames, pickKeyNames)
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal (withConnection)
import PgSchema.Schema
import PgSchema.Types
import Data.String
import PgSchema.Utils.Internal
import Prelude as P


data InsertMode = Insert | Upsert | Update deriving (Eq, Show)


-- | Insert records into a table and its children using JSON data internally.
--
-- Uses the same SQL pipeline as 'upsertJSON', but each row is a plain @INSERT@
-- (no @UPDATE@, no @INSERT … ON CONFLICT …@). Any duplicate primary or unique key
-- raises a PostgreSQL error. Requires all mandatory columns at every node
-- ('InsertTreeReturning'; 'ReturningMatchesInsert' on @r'@).
--
-- /@json@ \/ @jsonb@ columns/: 'Nothing' is stored as SQL @NULL@ (as in
-- 'PgSchema.DML.insertSch'). Rows travel to PostgreSQL as one JSON document, where
-- 'Nothing' and a JSON @null@ value (e.g. @Just 'A.Null'@ in a
-- @Maybe 'A.Value'@ field) are both encoded as @null@, so a JSON @null@ value
-- is stored as SQL @NULL@ too and is read back as 'Nothing'. The same holds
-- for 'upsertJSON' and 'updateJSON' (including the @SET@ part of an update or
-- upsert). Use 'PgSchema.DML.Insert.insertSch' \/
-- 'PgSchema.DML.Update.updateByCond' if you need to store the JSON
-- value @null@ itself.
insertJSON
  :: forall ann -> forall r r'. InsertTreeReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
insertJSON ann @r @r' conn rs = insertJSONImpl ann @r @r' conn rs Insert

-- | Like 'insertJSON', but does not return rows.
--
insertJSON_
  :: forall ann -> forall r. InsertTreeNonReturning ann r
  => Connection -> [r] -> IO Text
insertJSON_ ann @r conn rs = insertJSONImpl_ ann @r conn rs Insert

-- | Upsert a forest of rows into the root table and its /child/ tables in one
-- round-trip, using JSON inside PostgreSQL (same pipeline as 'insertJSON').
--
-- The input has the same shape as for 'insertJSON': child rows are given in list
-- fields named after the foreign keys, and their foreign-key columns are taken
-- from the parent row automatically. Every node must have all mandatory columns
-- or a full primary / unique key ('UpsertTreeReturning'). The operation is chosen
-- for each node separately:
--
-- * not all mandatory columns are present: @UPDATE@ by key (the row may be not
--   found; then its children are skipped);
-- * all mandatory columns and a full key are present:
--   @INSERT … ON CONFLICT (key) DO UPDATE@ of the other given columns, or
--   @INSERT … ON CONFLICT DO NOTHING@ if there are no other columns (an existing
--   row with this key is then looked up within the parent);
-- * all mandatory columns, but no key: plain @INSERT@.
--
-- The key is the first fully covered one of: the primary key, unique keys over
-- @NOT NULL@ columns, other unique keys.
--
-- Returning list elements must be bare rows where the node is always inserted or
-- upserted, and @Maybe@ where the row may be not found ('ReturningMatchesUpsert').
--
-- 'Nothing' (and a JSON @null@ value) for a @json@ \/ @jsonb@ column is
-- stored as SQL @NULL@; see 'insertJSON'.
upsertJSON
  :: forall ann -> forall r r'. UpsertTreeReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
upsertJSON ann @r @r' conn rs = insertJSONImpl ann @r @r' conn rs Upsert

-- | Like 'upsertJSON', but does not return rows.
--
upsertJSON_
  :: forall ann -> forall r. UpsertTreeNonReturning ann r
  => Connection -> [r] -> IO Text
upsertJSON_ ann @r conn rs = insertJSONImpl_ ann @r conn rs Upsert

-- | Update an existing forest (never @INSERT@). Returning type @r'@ must satisfy
-- 'ReturningMatchesUpdate' (@Maybe@ on each child list element). Flat
-- 'PgSchema.DML.updateByKey' instead uses bare @r'@ and returns @IO ([Maybe r'], Text)@.
--
-- 'Nothing' (and a JSON @null@ value) for a @json@ \/ @jsonb@ column sets it
-- to SQL @NULL@; see 'insertJSON'.
updateJSON
  :: forall ann -> forall r r'. UpdateTreeReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
updateJSON ann @r @r' conn rs = insertJSONImpl ann @r @r' conn rs Update

-- | Like 'updateJSON', but does not return rows.
--
updateJSON_
  :: forall ann -> forall r. UpdateTreeNonReturning ann r
  => Connection -> [r] -> IO Text
updateJSON_ ann @r conn rs = insertJSONImpl_ ann @r conn rs Update

insertJSONImpl
  :: forall ann -> forall r r'. (HasSchema ann, TreeIn ann r, TreeOut ann r')
  => Connection -> [r] -> InsertMode -> IO ([r'], Text)
insertJSONImpl ann @r @r' conn rs mode = withTransactionIfNot conn do
  let sql' = T.unpack sql in trace' sql' $ void $ execute_ conn $ fromString sql'
  [Only res] <- let q = "select pg_temp.__ins(?)" in
    traceShow' q
      $ trace' (BSL.unpack $ A.encode (PgTag @ann @r <$> rs))
      $ query conn q $ Only $ PgTag @ann @r <$> rs
  void $ execute_ conn "drop function pg_temp.__ins"
  pure (unPgTag @ann @r' <$> res, sql)
  where
    sql = insertJSONText' mode (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
      (getRecordInfo @ann @r) (getRecordInfo @ann @r').fields

withTransactionIfNot :: Connection -> IO a -> IO a
withTransactionIfNot conn act = do
  stat <- withConnection conn PQ.transactionStatus
  case stat of
    PQ.TransIdle -> withTransaction conn act
    PQ.TransInTrans -> act
    PQ.TransInError -> act
    PQ.TransActive ->
      error "PgSchema.DML.InsertJSON.withTransactionIfNot: connection is active"
    PQ.TransUnknown ->
      error "PgSchema.DML.InsertJSON.withTransactionIfNot: \
        \unknown transaction status"

insertJSONImpl_
  :: forall ann -> forall r. (HasSchema ann, TreeIn ann r)
  => Connection -> [r] -> InsertMode -> IO Text
insertJSONImpl_ ann @r conn rs mode = withTransactionIfNot conn do
  void $ trace' (T.unpack sql) $ execute_ conn $ fromString $ T.unpack sql
  void $ execute conn "call pg_temp.__ins(?)" $ Only $ PgTag @ann @r <$> rs
  sql <$ execute_ conn "drop procedure pg_temp.__ins"
  where
    sql = insertJSONText' mode (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
      (getRecordInfo @ann @r) []

insertJSONText_ :: forall ann -> forall r s.
  (IsString s, Monoid s, Ord s, HasSchema ann, CRecInfo ann r) => s
insertJSONText_ ann @r =
  insertJSONText' Insert (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) []

insertJSONText :: forall ann -> forall r r'.
  ( HasSchema ann, CRecInfo ann r, CRecInfo ann r'
  , IsString s, Monoid s, Ord s ) => s
insertJSONText ann @r @r' =
  insertJSONText' Insert (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) (getRecordInfo @ann @r').fields

upsertJSONText_ :: forall ann -> forall r s.
  (IsString s, Monoid s, Ord s, HasSchema ann, CRecInfo ann r) => s
upsertJSONText_ ann @r =
  insertJSONText' Upsert (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) []

upsertJSONText :: forall ann -> forall r r'.
  ( HasSchema ann, CRecInfo ann r, CRecInfo ann r'
  , IsString s, Monoid s, Ord s ) => s
upsertJSONText ann @r @r' =
  insertJSONText' Upsert (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) (getRecordInfo @ann @r').fields

updateJSONText_ :: forall ann -> forall r s.
  (IsString s, Monoid s, Ord s, HasSchema ann, CRecInfo ann r) => s
updateJSONText_ ann @r =
  insertJSONText' Update (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) []

updateJSONText :: forall ann -> forall r r'.
  ( HasSchema ann, CRecInfo ann r, CRecInfo ann r'
  , IsString s, Monoid s, Ord s ) => s
updateJSONText ann @r @r' =
  insertJSONText' Update (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) (getRecordInfo @ann @r').fields

insertJSONText'
  :: forall s. (IsString s, Monoid s, Ord s)
  => InsertMode
  -> M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo Text
  -> [FieldInfo Text] -> s
insertJSONText' mode mapTypes mapTabs ir qfs = unlines'
  [ maybe
    "create or replace procedure pg_temp.__ins(data_0 jsonb) as $$"
    (const "create or replace function pg_temp.__ins(data_0 jsonb) returns jsonb as $$")
    mbRes
  , "declare"
  , unlines' $ ("  " <>) <$> decl
  , "begin"
  , unlines' body
  , maybe "" (\r ->"  return to_jsonb(" <> r <> ");") mbRes
  , "end; "
  , "$$ language plpgsql;" ]
  where
    (mbRes, (decl, body)) =
      evalRWS (insertJSONTextM mode mapTypes mapTabs ir qfs [] [] [])
        ("  ",0) 0

type MonadInsert s = RWS (s, Int) ([s],[s]) Int

data Field v = Field
  { jsonName :: Text
  , dbName :: Text
  , info :: v }
  deriving (Eq, Show)

insertJSONTextM
  :: forall s. (IsString s, Monoid s, Ord s)
  => InsertMode
  -> M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo Text
  -> [FieldInfo Text] -> [s] -> [s] -> [Text] -> MonadInsert s (Maybe s)
insertJSONTextM mode mapTypes mapTabs ri qfs fromFields toVars parentDbKeys = do
  (spaces, n) <- ask
  let
    sn = show' n
    dataN = "data_" <> sn
    rowN  = "row_" <> sn
    foundN = "found_" <> sn
    mbArrN = ("arr_" <> sn) <$ guard (not $ P.null qfs)
    decs = (if n == 0 then P.id else (dataN <> " jsonb;" :))
      (foundN <> " boolean;" : [rowN <> " record;"])
      <> foldMap (pure . (<> " jsonb[];")) mbArrN <> qretDecls
    qretDecls = qretPairs <&> \(fld, typ) -> fld <> sn <> " " <> typ <> "; "
    initArray = foldMap (pure . (<> ":= '{}';")) mbArrN
    startLoop =
      ["for " <> rowN <> " in select * from jsonb_array_elements("
      <> dataN <> ")", "loop"]
    keyOnlyInput =
      let
        mbTab = mapTabs M.!? ri.tabName
        mflds = foldMap (fmap fromText . mandatoryDbNames) mbTab
        srcFldsLoc = parentDbKeys <> ((.dbName) <$> iplains)
      in not $ P.null $ mflds L.\\ srcFldsLoc
    keyNamesTxt = case mapTabs M.!? ri.tabName of
      Just ti ->
        fromMaybe [] $ pickKeyNames ti (parentDbKeys <> ((.dbName) <$> iplains))
      Nothing -> []
    (ins, mayMiss) = resolve
      where
        jsonFld ip = case mapTypes M.!? ip.info.fdType of
          Just (TypDef "A" (Just t) _) ->
            "case when jsonb_typeof(" <> rowN <> ".value->'" <> fld <> "') = 'array'"
            <> " then (select coalesce(array_agg(__x)::" <> fromText (qualName t) <> "[], '{}')"
            <> " from jsonb_array_elements_text(" <> rowN <> ".value->'" <> fld <> "') __x) else null end"
          _
            -- @->@ gives the jsonb value @'null'@ for @"fld": null@ (how
            -- 'Nothing' is encoded); turn it into SQL @NULL@. @row_N.value@
            -- is always @jsonb@, so the comparison works for @json@ columns
            -- too (stored via the jsonb -> json assignment cast).
            | tn == "json" || tn == "jsonb" ->
              "nullif(" <> rowN <> ".value->'" <> fld <> "', 'null'::jsonb)"
            | otherwise                     -> "(" <> rowN <> ".value->>'" <> fld <> "')::" <> ty
          where
            fld = fromText ip.jsonName
            ty  = fromText (qualName ip.info.fdType)
            tn  = ip.info.fdType.nnsName
        plains = iplains <&> \ip -> (fromText ip.dbName, jsonFld ip)
        srcFlds = fromFields <> (fst <$> plains)
        srcVars = toVars <> (jsonFld <$> iplains)
        srcMap = M.fromList $ P.zip srcFlds srcVars
        mbSetVars
          | P.null (qretFlds L.\\ srcFlds) = Just
            $ catMaybes $ P.zipWith (\fld var ->
              (\srcVar -> var <> " := " <> srcVar <> ";")
              <$> srcMap M.!? fld) qretFlds qretVars
          | otherwise = Nothing
        ins0 =
          [ "  insert into " <> qualTabName <> "(" <> intercalate' ", " srcFlds <> ")"
          , "    values (" <> intercalate' ", " srcVars <> ")"]
        keyNames = fromText <$> keyNamesTxt
        (plainsKey, plainsOthers) =
          L.partition ((`L.elem` keyNames) . fst) plains
        sWhere = "where " <> intercalate' " and "
            ( L.zipWith nameVal fromFields toVars
            <> (uncurry keyValDistinct <$> plainsKey))
        upd0 =
          [ "  update " <> qualTabName
          , "    set " <> intercalate' ", " (uncurry nameVal <$> plains)
          , "    " <> sWhere]
        qretVars = (<> sn) <$> qretFlds
        rets
          | noRets = []
          | otherwise = ["    returning " <> intercalate' ", " qretFlds
            <> " into " <> intercalate' ", " qretVars]
        ups0 conflictCols
          | L.null plainsOthers = case mbSetVars of
            -- on conflict the row with this key must belong to this parent:
            -- otherwise it is not found (@mayMiss@)
            Just xs -> ins0 <> [ "    on conflict do nothing;"]
              <> ["  if not found then"
                , "    perform 1 from " <> qualTabName
                , "      " <> sWhere <> ";"
                , "  end if;"]
              <> ["  " <> intercalate' " " xs]
            Nothing -> ins0 <> addSemiColon ([ "    on conflict do nothing"] <> rets)
              <> ["  if not found then"
                , "    select " <> intercalate' ", " qretFlds <> " into "
                  <> intercalate' ", " qretVars
                , "      from " <> qualTabName
                , "      " <> sWhere <> ";"
                , "  end if;"]
          | P.null conflictCols = addSemiColon ins0
          | otherwise = addSemiColon $ ins0
            <> [ "    on conflict (" <> intercalate' ", " conflictCols <> ")"
              , "      do update set " <> intercalate' ", "
                  (plainsOthers <&> \(name, _) -> name <> " = EXCLUDED." <> name) ]
            <> rets
        -- @mayMiss@: the row may be not found (@UPDATE@ by key, or
        -- @ON CONFLICT DO NOTHING@ with the key of a row of another parent);
        -- then its children are skipped and @null@ is returned for it.
        resolve
          | mode == Update =
            (addSemiColon (upd0 <> rets), True)
          | mode == Insert =
            (addSemiColon (ins0 <> rets), False)
          | keyOnlyInput =
            (addSemiColon (upd0 <> rets), True)
          | not $ P.null keyNamesTxt =
            (ups0 keyNames, L.null plainsOthers)
          | otherwise = (addSemiColon (ins0 <> rets), False)
    endLoop = "end loop;"
    processChildren = do
      (spaces', _) <- ask
      (mapMaybe sequenceA -> arrs) <- for ichildren \(child, childRi) -> do
        modify (+1)
        n' <- get
        tell (mempty, pure $ spaces' <> "data_" <> show' n' <> " := "
          <> rowN <> ".value->'" <> fromText child.jsonName <> "';")
        let
          qfs' = foldMap ((.fields) . snd)
            $ L.find (\(qc, _) -> qc.jsonName == child.jsonName) qchildren
        let parentKeys = nub $ P.map (.fromName) child.info
        mbArr <- local (second $ const n') $ insertJSONTextM mode
          mapTypes mapTabs
          childRi qfs' (fromText . (.fromName) <$> child.info)
          ((<> sn) . fromText . (.toName) <$> child.info)
          parentKeys
        pure (fromText child.jsonName, mbArr)
      pure arrs
    appendReturning arrN arrs =
      let
        jsonFlds = intercalate' ", "
          $ (qplains <&> \qp -> "'" <> fromText qp.jsonName
            <> "', " <> fromText qp.dbName <> sn)
          <> (arrs <&> \(jsonN, arr) -> "'" <> jsonN <> "', to_jsonb(" <> arr <> ")")
        obj = "jsonb_build_object(" <> jsonFlds <> ")"
      in if mayMiss
        then
          [ spaces <> "  if " <> foundN <> " then"
          , spaces <> "    " <> arrN <> " := array_append(" <> arrN <> ", " <> obj <> ");"
          , spaces <> "  else"
          , spaces <> "    " <> arrN <> " := array_append("
            <> arrN <> ", 'null'::jsonb);"
          , spaces <> "  end if;" ]
        else
          [ spaces <> "  " <> arrN <> " := array_append(" <> arrN <> ", " <> obj <> ");" ]
  tell (decs, fmap (spaces <>) $ initArray <> startLoop <> ins
    <> [spaces <> "  " <> foundN <> " := found;"])
  arrs <- if mayMiss
    then do
      tell (mempty, pure $ spaces <> "  if " <> foundN <> " then")
      ch <- local (first ("    " <>)) processChildren
      tell (mempty, pure $ spaces <> "  end if;")
      pure ch
    else local (first ("  " <>)) processChildren
  case mbArrN of
    Nothing -> pure ()
    Just arrN -> tell (mempty, appendReturning arrN arrs)
  tell (mempty, pure $ spaces <> endLoop)
  pure mbArrN
  where
    splitFields = P.foldr (\fi -> case fi.fieldKind of
      (RFPlain fd)  -> first (Field fi.fieldName fi.fieldDbName fd :)
      (RFToHere ri' refs) -> second ((Field fi.fieldName fi.fieldDbName refs, ri') :)
      _ -> P.id) mempty
    (iplains, ichildren) = splitFields ri.fields
    (qplains, qchildren) = splitFields qfs
    qualTabName = fromText (qualName ri.tabName)
    qcFlds = fmap ((,) <$> (.toName) <*> qualName . (.toDef.fdType))
      $ nubBy ((==) `on` (.toName)) $ ichildren >>= (\x -> (fst x).info)
    qpFlds = qplains <&> \p -> (p.dbName, qualName p.info.fdType)
    qretPairs = fmap (bimap fromText fromText)
      $ nubBy ((==) `on` fst) $ qcFlds <> qpFlds
    nameVal name val = name <> " = " <> val
    keyValDistinct name val = name <> " IS NOT DISTINCT FROM " <> val
    noRets = P.null qplains && P.null ichildren
    qretFlds = fst <$> qretPairs
    addSemiColon = \case
      [] -> []
      [x] -> [x <> ";"]
      (x:xs) -> x : addSemiColon xs
