module Database.PostgreSQL.DML.InsertJSON where

import Control.Monad
import Control.Monad.RWS
import Data.Aeson as A
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


insertJSON :: forall r r'. forall sch t ->
  InsertReturning sch t r r' => Connection -> [r] -> IO ([r'], Text)
insertJSON sch t conn rs = withTransactionIfNot conn do
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

insertJSON_
  :: forall r. forall  sch t -> (InsertNonReturning sch t r, ToJSON r)
  => Connection -> [r] -> IO Text
insertJSON_ sch t conn rs = withTransactionIfNot conn do
  void $ trace' (T.unpack sql) $ execute_ conn $ fromString $ T.unpack sql
  void $ execute conn "call pg_temp.__ins(?)" $ Only $ SchList rs
  sql <$ execute_ conn "drop procedure pg_temp.__ins"
  where
    sql = insertJSONText_ @r sch t

insertJSONText_ :: forall r s. forall sch t ->
  (IsString s, Monoid s, InsertNonReturning sch t r, ToJSON r) => s
insertJSONText_ sch t =
  insertJSONText' @s (typDefMap @sch) (getRecordInfo @sch @t @r) []

insertJSONText :: forall r r' s. forall sch t ->
  (IsString s, Monoid s, InsertReturning sch t r r', ToJSON r) => s
insertJSONText sch t = insertJSONText' (typDefMap @sch)
  (getRecordInfo @sch @t @r) (getRecordInfo @sch @t @r').fields

insertJSONText'
  :: forall s. (IsString s, Monoid s)
  => M.Map NameNS TypDef -> RecordInfo -> [FieldInfo RecordInfo] -> s
insertJSONText' mapTypes ir qfs = unlines'
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
      evalRWS (insertJSONTextM mapTypes ir qfs [] []) ("  ",0) 0

type MonadInsert s = RWS (s, Int) ([s],[s]) Int
insertJSONTextM
  :: forall s. (IsString s, Monoid s)
  => M.Map NameNS TypDef -> RecordInfo -> [FieldInfo RecordInfo] -> [s] -> [s]
  -> MonadInsert s (Maybe s)
insertJSONTextM mapTypes ri qfs fromFields toVars = do
  (spaces, n) <- ask
  let
    sn = show' n
    dataN = "data_" <> sn
    rowN  = "row_" <> sn
    mbArrN = ("arr_" <> sn) <$ guard (not $ P.null qfs)
    qcFlds = fmap ((,) <$> (.toName) <*> qualName . (.toDef.fdType))
      $ nubBy ((==) `on` (.toName)) $ ichildren >>= snd . fst
    qpFlds = ((,) <$> fst <*> qualName . (.fdType) . snd) <$> qplains
    qretPairs = fmap (bimap fromText fromText)
      $ nubBy ((==) `on` fst) $ qcFlds <> qpFlds
    qretDecls = qretPairs <&> \(fld, typ) -> fld <> sn <> " " <> typ <> "; "
    decs = (if n == 0 then P.id else (dataN <> " jsonb;" :))
      [rowN <> " record;"]
      <> foldMap (pure . (<> " jsonb[];")) mbArrN <> qretDecls
    initArray = foldMap (pure . (<> ":= '{}';")) mbArrN
    startLoop =
      ["for " <> rowN <> " in select * from jsonb_array_elements("
      <> dataN <> ")", "loop"]
    ins = ["  insert into " <> fromText (qualName ri.tabName) <> "("
      <> intercalate' ", " (fromFields <> (fromText . fst <$> iplains))
      <> ")", "    values (" <> intercalate' ", "
        (toVars <> (jsonFld <$> iplains)) <> ")" <> if noRets then ";" else ""]
        <> rets
      where
        noRets = P.null qplains && P.null ichildren
        qretFlds = fst <$> qretPairs
        qretVars = (<> sn) <$> qretFlds
        jsonFld (dbn,def) = case mapTypes M.!? def.fdType of
          Just (TypDef "A" (Just t) _) ->
            "translate(" <> rowN <> ".value->>'" <> fromText dbn
              <> "'::text, '[]', '{}')::" <> fromText (qualName t) <> "[]"
          _ ->
            "(" <> rowN <> ".value->>'" <> fromText dbn <> "')::"
              <> fromText (qualName def.fdType)
        rets
          | P.null qplains && P.null ichildren = []
          | otherwise = ["    returning " <> intercalate' ", " qretFlds
            <> " into " <> intercalate' ", " qretVars <> ";"]
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
    mbArr <- local (second $ const n') $ insertJSONTextM mapTypes (snd ic) qfs'
      (fromText . (.fromName) <$> snd (fst ic))
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
