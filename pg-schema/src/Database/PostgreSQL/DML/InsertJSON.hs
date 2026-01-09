{- HLINT ignore "Eta reduce" -}
module Database.PostgreSQL.DML.InsertJSON
  ( insertJSON, insertJSON_, upsertJSON, upsertJSON_
  , insertJSONText, insertJSONText_ ) where

import Control.Monad
import Control.Monad.RWS
import Data.Bifunctor
import Data.Foldable as F
import Data.Function
import Data.Functor
import Data.List as L
import Data.Map as M hiding (mapMaybe)
import Data.Maybe
import Data.Text as T hiding (any)
import Data.Traversable
import Database.PostgreSQL.DML.Insert.Types
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec
import Database.Schema.ShowType (qualName)
import Database.Types.SchList
import Data.String
import GHC.Int
import PgSchema.Util
import Prelude as P


insertJSON
  :: forall r r'. forall sch t
    -> (SrcJSON sch t r, TgtJSON sch t r', AllMandatory sch t r '[])
  => Connection -> [r] -> IO ([r'], Text)
insertJSON sch t = insertJSONImpl sch t

insertJSON_
  :: forall r. forall sch t -> (SrcJSON sch t r, AllMandatory sch t r '[])
  => Connection -> [r] -> IO Text
insertJSON_ sch t = insertJSONImpl_ sch t

upsertJSON
  :: forall r r'. forall sch t
    -> (SrcJSON sch t r, TgtJSON sch t r', AllMandatoryOrHasPK sch t r '[])
  => Connection -> [r] -> IO ([r'], Text)
upsertJSON sch t = insertJSONImpl sch t

upsertJSON_
  :: forall r. forall sch t -> (SrcJSON sch t r, AllMandatoryOrHasPK sch t r '[])
  => Connection -> [r] -> IO Text
upsertJSON_ sch t = insertJSONImpl_ sch t

insertJSONImpl :: forall r r'. forall sch t ->
  (SrcJSON sch t r, TgtJSON sch t r') => Connection -> [r] -> IO ([r'], Text)
insertJSONImpl sch t conn rs = withTransactionIfNot conn do
  let sql' = T.unpack sql in trace' sql' $ void $ execute_ conn $ fromString sql'
  [Only (SchList res)] <- let q = "select pg_temp.__ins(?)" in
    traceShow' q $ query conn q $ Only $ SchList rs
  void $ execute_ conn "drop function pg_temp.__ins"
  pure (res, sql)
  where
    sql = insertJSONText @r @r' sch t

withTransactionIfNot :: Connection -> IO a -> IO a
withTransactionIfNot conn act = do
  isInTrans <- any (isJust @Int64 . fromOnly)
    <$> query_ conn "SELECT txid_current_if_assigned()"
  (if isInTrans then id else withTransaction conn) act

insertJSONImpl_
  :: forall r. forall  sch t -> SrcJSON sch t r
  => Connection -> [r] -> IO Text
insertJSONImpl_ sch t conn rs = withTransactionIfNot conn do
  void $ trace' (T.unpack sql) $ execute_ conn $ fromString $ T.unpack sql
  void $ execute conn "call pg_temp.__ins(?)" $ Only $ SchList rs
  sql <$ execute_ conn "drop procedure pg_temp.__ins"
  where
    sql = insertJSONText_ @r sch t

insertJSONText_ :: forall r s. forall sch t ->
  (IsString s, Monoid s, SrcJSON sch t r, Ord s) => s
insertJSONText_ sch t = insertJSONText' @s (typDefMap @sch) (tabInfoMap @sch)
  (getRecordInfo @sch @t @r) []

insertJSONText :: forall r r' s. forall sch t ->
  (SrcJSON sch t r, TgtJSON sch t r', IsString s, Monoid s, Ord s) => s
insertJSONText sch t = insertJSONText' (typDefMap @sch) (tabInfoMap @sch)
  (getRecordInfo @sch @t @r) (getRecordInfo @sch @t @r').fields

insertJSONText'
  :: forall s. (IsString s, Monoid s, Ord s)
  => M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo -> [FieldInfo RecordInfo] -> s
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

insertJSONTextM
  :: forall s. (IsString s, Monoid s, Ord s)
  => M.Map NameNS TypDef -> M.Map NameNS TabInfo -> RecordInfo
  -> [FieldInfo RecordInfo] -> [s] -> [s] -> MonadInsert s (Maybe s)
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
    ins = case mbKeyMand of
      Just (pk, mflds)
        | not $ P.null $ mflds L.\\ (fromFields <> (fst <$> plains)) ->
          addSemiColon (upd0 <> rets)
        | P.null $ pk L.\\ (fromFields <> (fst <$> plainsPK))        -> ups0 pk
      _ -> addSemiColon (ins0 <> rets)
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
          | otherwise = ins0 <> addSemiColon
            [ "    on conflict (" <> intercalate' ", " pk <> ")"
            , "      do update set " <> intercalate' ", " (plainsOthers <&>
                \(name, _) -> name <> " = " <> "EXCLUDED." <> name) ]
        qretVars = (<> sn) <$> qretFlds
        plains = iplains <&> \ip -> (fromText (fst ip), jsonFld ip)
        (plainsPK, plainsOthers) = L.partition ((`L.elem` foldMap fst mbKeyMand) . fst) plains
        jsonFld (dbn,def) = case mapTypes M.!? def.fdType of
          Just (TypDef "A" (Just t) _) ->
            "translate(" <> rowN <> ".value->>'" <> fromText dbn
              <> "'::text, '[]', '{}')::" <> fromText (qualName t) <> "[]"
          _ ->
            "(" <> rowN <> ".value->>'" <> fromText dbn <> "')::"
              <> fromText (qualName def.fdType)
        rets
          | noRets = []
          | otherwise = ["    returning " <> intercalate' ", " qretFlds
            <> " into " <> intercalate' ", " qretVars]
    endLoop = "end loop;"
  tell (decs, fmap (spaces <>) $ initArray <> startLoop <> ins)
  (mapMaybe sequenceA -> arrs) <- local (first ("  " <>)) $ for ichildren \ic -> do
    modify (+1)
    n' <- get
    tell (mempty, pure $ spaces <> "  data_" <> show' n' <> " := "
      <> rowN <> ".value->>'" <> fromText (fst $ fst ic) <> "';")
    let
      qfs' = foldMap ((.fields) . snd)
        $ L.find (\qc -> ((==) `on` (fst . fst)) qc ic) qchildren
    mbArr <- local (second $ const n') $ insertJSONTextM mapTypes mapTabs
      (snd ic) qfs' (fromText . (.fromName) <$> snd (fst ic))
      ((<> sn) . fromText . (.toName) <$> snd (fst ic))
    pure (fromText (fst $ fst ic), mbArr)
  let
    appendArray = foldMap (\arrN -> pure $ "  " <> arrN <> ":= array_append("
      <> arrN <> ", jsonb_build_object(" <> jsonFlds <> "));") mbArrN
      where
        jsonFlds = intercalate' ", "
          $ (qplains <&> \qp -> let fld = fromText (fst qp) in
            "'" <> fld <> "', " <> fld <> sn)
          <> (arrs <&> \(dbn, arr) -> "'" <> dbn <> "', to_jsonb(" <> arr <> ")")
  tell (mempty, fmap (spaces <>) $ appendArray <> [endLoop] )
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
