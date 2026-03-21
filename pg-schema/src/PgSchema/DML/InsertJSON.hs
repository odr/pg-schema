{-# LANGUAGE OverloadedRecordDot #-}
module PgSchema.DML.InsertJSON
  ( insertJSON, insertJSON_, upsertJSON, upsertJSON_
  , insertJSONText, insertJSONText_ ) where

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
import Database.PostgreSQL.Simple
import PgSchema.Schema
import PgSchema.Types
import Data.String
import GHC.Int
import PgSchema.Utils.Internal
import Prelude as P


-- | Insert records into table and its children using JSON data internally.
--
-- | Like 'upsertJSON', but requires all mandatory columns at every node (insert-only constraint).

insertJSON
  :: forall ann -> forall r r'. InsertTreeReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
insertJSON ann @r @r' = insertJSONImpl ann @r @r'

-- | Like 'insertJSON', but does not return rows.
--
insertJSON_
  :: forall ann -> forall r. InsertTreeNonReturning ann r
  => Connection -> [r] -> IO Text
insertJSON_ ann @r = insertJSONImpl_ ann @r

-- | Upsert a forest of rows into the root table and its /child/ tables in one
-- round-trip, using JSON inside PostgreSQL (same pipeline as 'insertJSON').
--
-- __Input shape (@r@):__ a record tree that may contain the root table’s columns
-- and nested /child/ branches (one-to-many from the root downward). There are no
-- nested /parent/ branches: parent keys are implied by the tree you send, not by
-- embedding parent rows inside children.
--
-- __Output shape (@r'@):__ a record tree whose graph of nested tables is a
-- /subgraph/ of the input: the same tables can appear, but you choose which
-- columns (and which levels) appear in the result—whatever is available through
-- the generated @RETURNING@/result projection. Field sets may differ from @r@;
-- relation structure cannot grow beyond what you sent in.
--
-- __What to supply at each node:__ at every level, each row must either include
-- all mandatory columns (for columns that are mandatory in the schema /sense of
-- this API/) or, alternatively, enough primary-key columns to identify an existing
-- row. Foreign-key columns that are filled in by the parent level (for example
-- after an auto-generated id on insert) do /not/ need to be present on the child
-- payload.
--
-- __Insert vs update vs upsert per row:__ the engine picks one of @INSERT@,
-- @UPDATE@, or @UPSERT@ from the keys and mandatory fields you provide:
--
-- * all mandatory fields present and /no/ primary key  →  @INSERT@
-- * primary key present, not all mandatory fields      →  @UPDATE@
-- * primary key present /and/ all mandatory fields    →  @UPSERT@
--
-- 'insertJSON' is the same execution path but adds a stricter type-level
-- constraint: /every/ mandatory field must be present (pure inserts). 'upsertJSON'
-- relaxes that so updates and upserts are expressible as in the rules above.
upsertJSON
  :: forall ann -> forall r r'. UpsertTreeReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
upsertJSON ann @r @r' = insertJSONImpl ann @r @r'

-- | Like 'upsertJSON', but does not return rows.
--
upsertJSON_
  :: forall ann -> forall r. UpsertTreeNonReturning ann r
  => Connection -> [r] -> IO Text
upsertJSON_ ann @r = insertJSONImpl_ ann @r

insertJSONImpl
  :: forall ann -> forall r r'. (TreeSch ann, TreeIn ann r, TreeOut ann r')
  => Connection -> [r] -> IO ([r'], Text)
insertJSONImpl ann @r @r' conn rs = withTransactionIfNot conn do
  let sql' = T.unpack sql in trace' sql' $ void $ execute_ conn $ fromString sql'
  [Only res] <- let q = "select pg_temp.__ins(?)" in
    traceShow' q
      $ trace' (BSL.unpack $ A.encode (PgTag @ann @r <$> rs))
      $ query conn q $ Only $ PgTag @ann @r <$> rs
  void $ execute_ conn "drop function pg_temp.__ins"
  pure (unPgTag @ann @r' <$> res, sql)
  where
    sql = insertJSONText ann @r @r'

withTransactionIfNot :: Connection -> IO a -> IO a
withTransactionIfNot conn act = do
  isInTrans <- any (isJust @Int64 . fromOnly)
    <$> query_ conn "SELECT txid_current_if_assigned()"
  (if isInTrans then id else withTransaction conn) act

insertJSONImpl_
  :: forall ann -> forall r. (TreeSch ann, TreeIn ann r)
  => Connection -> [r] -> IO Text
insertJSONImpl_ ann @r conn rs = withTransactionIfNot conn do
  void $ trace' (T.unpack sql) $ execute_ conn $ fromString $ T.unpack sql
  void $ execute conn "call pg_temp.__ins(?)" $ Only $ PgTag @ann @r <$> rs
  sql <$ execute_ conn "drop procedure pg_temp.__ins"
  where
    sql = insertJSONText_ ann @r

insertJSONText_ :: forall ann -> forall r s.
  (IsString s, Monoid s, Ord s, TreeSch ann, CRecInfo ann r) => s
insertJSONText_ ann @r = insertJSONText' (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
  (getRecordInfo @ann @r) []

insertJSONText :: forall ann -> forall r r'.
  ( TreeSch ann, CRecInfo ann r, CRecInfo ann r'
  , IsString s, Monoid s, Ord s ) => s
insertJSONText ann @r @r' =
  insertJSONText' (typDefMap @(AnnSch ann)) (tabInfoMap @(AnnSch ann))
    (getRecordInfo @ann @r) (getRecordInfo @ann @r').fields

insertJSONText'
  :: forall s. (IsString s, Monoid s, Ord s)
  => M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo Text -> [FieldInfo Text] -> s
insertJSONText' mapTypes mapTabs ir qfs = unlines'
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
      evalRWS (insertJSONTextM mapTypes mapTabs ir qfs [] []) ("  ",0) 0

type MonadInsert s = RWS (s, Int) ([s],[s]) Int
-- R: (leading spaces to format code, number of table in tree)
-- W: (lines of declarations, lines of function body)
-- S: maximum number of table in tree "in use"

data OP = INS | UPD | UPS deriving (Eq, Show)

data Field v = Field
  { jsonName :: Text
  , dbName :: Text
  , info :: v }
  deriving (Eq, Show)

insertJSONTextM
  :: forall s. (IsString s, Monoid s, Ord s)
  => M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo Text
  -> [FieldInfo Text] -> [s] -> [s] -> MonadInsert s (Maybe s)
insertJSONTextM mapTypes mapTabs ri qfs fromFields toVars = do
  (spaces, n) <- ask
  let
    sn = show' n
    dataN = "data_" <> sn
    rowN  = "row_" <> sn
    mbArrN = ("arr_" <> sn) <$ guard (not $ P.null qfs)
    decs = (if n == 0 then P.id else (dataN <> " jsonb;" :))
      [rowN <> " record;"]
      <> foldMap (pure . (<> " jsonb[];")) mbArrN <> qretDecls
    qretDecls = qretPairs <&> \(fld, typ) -> fld <> sn <> " " <> typ <> "; "
    initArray = foldMap (pure . (<> ":= '{}';")) mbArrN
    startLoop =
      ["for " <> rowN <> " in select * from jsonb_array_elements("
      <> dataN <> ")", "loop"]
    (ins, op) = case mbKeyMand of
      Just (pk, mflds)
        | not $ P.null $ mflds L.\\ (fromFields <> (fst <$> plains)) ->
          (addSemiColon (upd0 <> rets), UPD)
        | P.null $ pk L.\\ (fromFields <> (fst <$> plainsPK)) -> (ups0 pk, UPS)
      _ -> (addSemiColon (ins0 <> rets), INS)
      where
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
        sWhere = "where " <> intercalate' " and "
            ( L.zipWith nameVal fromFields toVars <> (uncurry nameVal <$> plainsPK))
        upd0 =
          [ "  update " <> qualTabName
          , "    set " <> intercalate' ", " (uncurry nameVal <$> plains)
          , "    " <> sWhere]
        ups0 pk
          | L.null plainsOthers = case mbSetVars of
            Just [] -> addSemiColon ins0
            Just xs -> ins0 <> [ "    on conflict do nothing;"]
              <> ["  " <> intercalate' " " xs]
            Nothing -> ins0 <> addSemiColon ([ "    on conflict do nothing"] <> rets)
              <> ["  if not found then"
                , "    select " <> intercalate' ", " qretFlds <> " into " <> intercalate' ", " qretVars
                , "      from " <> qualTabName
                , "      " <> sWhere <> ";"
                , "  end if;"]
          | L.null pk = addSemiColon ins0 -- support for tables without PK
          | otherwise = addSemiColon $ ins0
            <> [ "    on conflict (" <> intercalate' ", " pk <> ")"
              , "      do update set " <> intercalate' ", " (plainsOthers <&>
                  \(name, _) -> name <> " = " <> "EXCLUDED." <> name) ]
            <> rets
        qretVars = (<> sn) <$> qretFlds
        plains = iplains <&> \ip -> (fromText ip.dbName, jsonFld ip)
        (plainsPK, plainsOthers) = L.partition ((`L.elem` foldMap fst mbKeyMand) . fst) plains
        jsonFld ip = case mapTypes M.!? ip.info.fdType of
          Just (TypDef "A" (Just t) _) ->
            "case when jsonb_typeof(" <> rowN <> ".value->'" <> fromText ip.jsonName <> "') = 'array'"
            <> " then (select coalesce(array_agg(__x)::" <> fromText (qualName t) <> "[], '{}')"
            <> " from jsonb_array_elements_text(" <> rowN <> ".value->'" <> fromText ip.jsonName <> "') __x) else null end"
          _ ->
            "(" <> rowN <> ".value->>'" <> fromText ip.jsonName <> "')::"
              <> fromText (qualName ip.info.fdType)
        rets
          | noRets = []
          | otherwise = ["    returning " <> intercalate' ", " qretFlds
            <> " into " <> intercalate' ", " qretVars]
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
        mbArr <- local (second $ const n') $ insertJSONTextM mapTypes mapTabs
          childRi qfs' (fromText . (.fromName) <$> child.info)
          ((<> sn) . fromText . (.toName) <$> child.info)
        pure (fromText child.jsonName, mbArr)
      let
        appendArray = foldMap (\arrN -> pure $ arrN <> ":= array_append("
          <> arrN <> ", jsonb_build_object(" <> jsonFlds <> "));") mbArrN
          where
            jsonFlds = intercalate' ", "
              $ (qplains <&> \qp -> "'" <> fromText qp.jsonName
              <> "', " <> fromText qp.dbName <> sn)
              <> (arrs <&> \(jsonN, arr) -> "'" <> jsonN <> "', to_jsonb(" <> arr <> ")")
      tell (mempty, fmap (spaces' <>) appendArray)
  tell (decs, fmap (spaces <>) $ initArray <> startLoop <> ins)
  case op of
    UPD -> do
      tell (mempty, pure $ spaces <> "  if found then")
      local (first ("    " <>)) processChildren
      tell (mempty, pure $ spaces <> "  end if;")
    _ -> local (first ("  " <>)) processChildren
  tell (mempty, pure $ spaces <> endLoop)
  pure mbArrN
  where
    splitFields = P.foldr (\fi -> case fi.fieldKind of
      (RFPlain fd)  -> first (Field fi.fieldName fi.fieldDbName fd :)
      (RFToHere ri' refs) -> second ((Field fi.fieldName fi.fieldDbName refs, ri') :)
      _ -> P.id) mempty
    (iplains, ichildren) = splitFields ri.fields
    (qplains, qchildren) = splitFields qfs
    mbKeyMand = mapTabs M.!? ri.tabName <&> ((,)
      <$> fmap fromText . (.tiDef.tdKey)
      <*> fmap fromText . M.keys
        . M.filter (\fd -> not $ fd.fdNullable || fd.fdHasDefault) . (.tiFlds))
    qualTabName = fromText (qualName ri.tabName)
    qcFlds = fmap ((,) <$> (.toName) <*> qualName . (.toDef.fdType))
      $ nubBy ((==) `on` (.toName)) $ ichildren >>= (\x -> (fst x).info)
    qpFlds = qplains <&> \p -> (p.dbName, qualName p.info.fdType)
    qretPairs = fmap (bimap fromText fromText)
      $ nubBy ((==) `on` fst) $ qcFlds <> qpFlds
    nameVal name val = name <> " = " <> val
    noRets = P.null qplains && P.null ichildren
    qretFlds = fst <$> qretPairs
    addSemiColon = \case
      [] -> []
      [x] -> [x <> ";"]
      (x:xs) -> x : addSemiColon xs
