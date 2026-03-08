{- HLINT ignore "Eta reduce" -}
module PgSchema.PostgreSQL.DML.InsertJSON
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
import PgSchema.PostgreSQL.DML.Insert.Types
import PgSchema.PostgreSQL.HList
import Database.PostgreSQL.Simple
import PgSchema.Schema.Def
import PgSchema.Schema.ShowType
import Data.String
import GHC.Int
import PgSchema.Utils
import Prelude as P


insertJSON
  :: forall ren sch t -> forall r r' h h'. InsertReturning ren sch t r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
insertJSON ren sch t = insertJSONImpl ren sch t

insertJSON_
  :: forall ren sch t -> forall r h. InsertNonReturning ren sch t r h
  => Connection -> [r] -> IO Text
insertJSON_ ren sch t = insertJSONImpl_ ren sch t

upsertJSON
  :: forall ren sch t -> forall r r' h h'. UpsertReturning ren sch t r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
upsertJSON ren sch t = insertJSONImpl ren sch t

upsertJSON_
  :: forall ren sch t -> forall r h. UpsertNonReturning ren sch t r h
  => Connection -> [r] -> IO Text
upsertJSON_ ren sch t = insertJSONImpl_ ren sch t

insertJSONImpl
  :: forall ren sch t -> forall r r' h h'
  . (SrcJSON ren sch t r h, TgtJSON ren sch t r' h')
  => Connection -> [r] -> IO ([r'], Text)
insertJSONImpl ren sch t @r @r' @h @h' conn rs = withTransactionIfNot conn do
  let sql' = T.unpack sql in trace' sql' $ void $ execute_ conn $ fromString sql'
  [Only res] <- let q = "select pg_temp.__ins(?)" in
    traceShow' q
      $ trace' (BSL.unpack $ A.encode (toHList @ren @sch @t <$> rs))
      $ query conn q $ Only $ toHList @ren @sch @t <$> rs
  void $ execute_ conn "drop function pg_temp.__ins"
  pure (fromHList @ren @sch @t <$> res, sql)
  where
    sql = insertJSONText ren sch t @r @r' @h @h'

withTransactionIfNot :: Connection -> IO a -> IO a
withTransactionIfNot conn act = do
  isInTrans <- any (isJust @Int64 . fromOnly)
    <$> query_ conn "SELECT txid_current_if_assigned()"
  (if isInTrans then id else withTransaction conn) act

insertJSONImpl_
  :: forall ren sch t -> forall r h. SrcJSON ren sch t r h
  => Connection -> [r] -> IO Text
insertJSONImpl_ ren sch t @r @h conn rs = withTransactionIfNot conn do
  void $ trace' (T.unpack sql) $ execute_ conn $ fromString $ T.unpack sql
  void $ execute conn "call pg_temp.__ins(?)" $ Only $ toHList @ren @sch @t <$> rs
  sql <$ execute_ conn "drop procedure pg_temp.__ins"
  where
    sql = insertJSONText_ ren sch t @r @h

insertJSONText_ :: forall ren sch t -> forall r h s.
  (IsString s, Monoid s, SrcJSON ren sch t r h, Ord s) => s
insertJSONText_ _ren sch t @_r @h = insertJSONText' (typDefMap @sch) (tabInfoMap @sch)
  (getRecordInfo @sch @t @h) []

insertJSONText :: forall ren sch t -> forall r r' h h' s.
  (SrcJSON ren sch t r h, TgtJSON ren sch t r' h', IsString s, Monoid s, Ord s) => s
insertJSONText _ren sch t @_r @_r' @h @h' = insertJSONText' (typDefMap @sch) (tabInfoMap @sch)
  (getRecordInfo @sch @t @h) (getRecordInfo @sch @t @h').fields

insertJSONText'
  :: forall s. (IsString s, Monoid s, Ord s)
  => M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo -> [FieldInfo] -> s
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

insertJSONTextM
  :: forall s. (IsString s, Monoid s, Ord s)
  => M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo
  -> [FieldInfo] -> [s] -> [s] -> MonadInsert s (Maybe s)
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
        plains = iplains <&> \ip -> (fromText (fst ip), jsonFld ip)
        (plainsPK, plainsOthers) = L.partition ((`L.elem` foldMap fst mbKeyMand) . fst) plains
        jsonFld (dbn,def) = case mapTypes M.!? def.fdType of
          Just (TypDef "A" (Just t) _) ->
            "case when jsonb_typeof(" <> rowN <> ".value->'" <> fromText dbn <> "') = 'array'"
            <> " then (select coalesce(array_agg(__x)::" <> fromText (qualName t) <> "[], '{}')"
            <> " from jsonb_array_elements_text(" <> rowN <> ".value->'" <> fromText dbn <> "') __x) else null end"
          _ ->
            "(" <> rowN <> ".value->>'" <> fromText dbn <> "')::"
              <> fromText (qualName def.fdType)
        rets
          | noRets = []
          | otherwise = ["    returning " <> intercalate' ", " qretFlds
            <> " into " <> intercalate' ", " qretVars]
    endLoop = "end loop;"
    processChildren = do
      (spaces', _) <- ask
      (mapMaybe sequenceA -> arrs) <- for ichildren \ic -> do
        modify (+1)
        n' <- get
        tell (mempty, pure $ spaces' <> "data_" <> show' n' <> " := "
          <> rowN <> ".value->'" <> fromText (fst $ fst ic) <> "';")
        let
          qfs' = foldMap ((.fields) . snd)
            $ L.find (\qc -> ((==) `on` (fst . fst)) qc ic) qchildren
        mbArr <- local (second $ const n') $ insertJSONTextM mapTypes mapTabs
          (snd ic) qfs' (fromText . (.fromName) <$> snd (fst ic))
          ((<> sn) . fromText . (.toName) <$> snd (fst ic))
        pure (fromText (fst $ fst ic), mbArr)
      let
        appendArray = foldMap (\arrN -> pure $ arrN <> ":= array_append("
          <> arrN <> ", jsonb_build_object(" <> jsonFlds <> "));") mbArrN
          where
            jsonFlds = intercalate' ", "
              $ (qplains <&> \qp -> let fld = fromText (fst qp) in
                "'" <> fld <> "', " <> fld <> sn)
              <> (arrs <&> \(dbn, arr) -> "'" <> dbn <> "', to_jsonb(" <> arr <> ")")
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
      (RFPlain fd)  -> first ((fi.fieldDbName, fd):)
      (RFToHere ri' refs) -> second (((fi.fieldDbName, refs), ri'):)
      _ -> P.id) mempty
    (iplains, ichildren) = splitFields ri.fields
    (qplains, qchildren) = splitFields qfs
    mbKeyMand = mapTabs M.!? ri.tabName <&> ((,)
      <$> fmap fromText . (.tiDef.tdKey)
      <*> fmap fromText . M.keys
        . M.filter (\fd -> not $ fd.fdNullable || fd.fdHasDefault) . (.tiFlds))
    qualTabName = fromText (qualName ri.tabName)
    qcFlds = fmap ((,) <$> (.toName) <*> qualName . (.toDef.fdType))
      $ nubBy ((==) `on` (.toName)) $ ichildren >>= snd . fst
    qpFlds = ((,) <$> fst <*> qualName . (.fdType) . snd) <$> qplains
    qretPairs = fmap (bimap fromText fromText)
      $ nubBy ((==) `on` fst) $ qcFlds <> qpFlds
    nameVal name val = name <> " = " <> val
    noRets = P.null qplains && P.null ichildren
    qretFlds = fst <$> qretPairs
    addSemiColon = \case
      [] -> []
      [x] -> [x <> ";"]
      (x:xs) -> x : addSemiColon xs
