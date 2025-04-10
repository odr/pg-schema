module Database.PostgreSQL.DML.InsertJSON where

import Control.Monad
import Control.Monad.RWS
import Data.Aeson as A
import Data.Bifunctor
import Data.Foldable as F
import Data.Function
import Data.Functor
import Data.List as L
import Data.Maybe
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
import Data.Typeable


insertJSON :: forall r r'. forall sch t ->
  (InsertReturning sch t r r', ToJSON r, FromJSON r', Typeable r') =>
  Connection -> [r] -> IO [r']
insertJSON sch t conn rs = withTransactionIfNot conn do
  void $ execute_ conn $ insertJSONText @r @r' sch t
  [Only (SchList res)] <- query conn "select pg_temp.__ins(?)" $ Only $ SchList rs
  void $ execute_ conn "drop function pg_temp.__ins"
  pure res

withTransactionIfNot :: Connection -> IO a -> IO a
withTransactionIfNot conn act = do
  isInTrans <- any (isJust @Int64 . fromOnly)
    <$> query_ conn "SELECT txid_current_if_assigned()"
  (if isInTrans then id else withTransaction conn) act

insertJSON_
  :: forall r. forall  sch t -> (InsertNonReturning sch t r, ToJSON r)
  => Connection -> [r] -> IO ()
insertJSON_ sch t conn rs = withTransactionIfNot conn do
  void $ execute_ conn $ insertJSONText_ @r sch t
  void $ execute conn "call pg_temp.__ins(?)" $ Only $ SchList rs
  void $ execute_ conn "drop procedure pg_temp.__ins"

insertJSONText_ :: forall r s. forall sch t  ->
  (IsString s, Monoid s, InsertNonReturning sch t r, ToJSON r) => s
insertJSONText_ sch t = insertJSONText' @s (getDmlRecord sch t r) []

insertJSONText :: forall r r' s. forall sch t ->
  (IsString s, Monoid s, InsertReturning sch t r r', ToJSON r) => s
insertJSONText sch t = insertJSONText' (getDmlRecord sch t r)
  (getQueryRecord sch t r').qFields

insertJSONText'
  :: forall s. (IsString s, Monoid s) => DmlRecord -> [QueryField] -> s
insertJSONText' ir qfs = unlines'
  [ maybe
    "create or replace procedure pg_temp.__ins(data_0 jsonb) as $$"
    (\_ -> "create or replace function pg_temp.__ins(data_0 jsonb) returns jsonb as $$")
    mbRes
  , "declare"
  , unlines' $ ("  " <>) <$> decl
  , "begin"
  , unlines' body
  , maybe "" (\r ->"  return to_jsonb(" <> r <> ");") mbRes
  , "end; "
  , "$$ language plpgsql;" ]
  where
    (mbRes, (decl, body)) = evalRWS (insertJSONTextM ir qfs [] []) ("  ",0) 0

type MonadInsert s = RWS (s, Int) ([s],[s]) Int
insertJSONTextM
  :: forall s. (IsString s, Monoid s)
  => DmlRecord -> [QueryField] -> [s] -> [s] -> MonadInsert s (Maybe s)
insertJSONTextM ir qfs fromFields toVars = do
  (spaces, n) <- ask
  let
    sn = show' n
    dataN = "data_" <> sn
    rowN  = "row_" <> sn
    mbArrN = ("arr_" <> sn) <$ guard (not $ P.null qfs)
    qcFlds = fmap ((,) <$> (.toName) <*> qualName . (.toDef.fdType))
      $ nubBy ((==) `on` (.toName)) $ ichildren >>= (.frRefs)
    qpFlds = ((,) <$> (.fpDbName) <*> qualName . (.fpFldDef.fdType)) <$> qplains
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
    ins = ["  insert into " <> fromText (qualName ir.iTableName) <> "("
      <> intercalate' ", "
        (fromFields <> (fromText . (.fpDbName) <$> iplains))
      <> ")", "    values (" <> intercalate' ", "
        (toVars <> (jsonFld <$> iplains)) <> ")" <> if noRets then ";" else ""]
        <> rets
      where
        noRets = P.null qplains && P.null ichildren
        qretFlds = fst <$> qretPairs
        qretVars = (<> sn) <$> qretFlds
        jsonFld fp = "(" <> rowN <> ".value->>'" <> fromText fp.fpDbName <> "')::"
          <> fromText (qualName fp.fpFldDef.fdType)
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
      <> rowN <> ".value->>'" <> fromText ic.frDbName <> "';")
    let
      qfs' = foldMap (.frRec.qFields)
        $ L.find (\qc -> qc.frDbName == ic.frDbName) qchildren
    mbArr <- local (second $ const n') $ insertJSONTextM ic.frRec qfs'
      (fromText . (.fromName) <$> ic.frRefs)
      ((<> sn) . fromText . (.toName) <$> ic.frRefs)
    pure (fromText ic.frDbName, mbArr)
  let
    appendArray = foldMap (\arrN -> pure $ "  " <> arrN <> ":= array_append("
      <> arrN <> ", jsonb_build_object(" <> jsonFlds <> "));") mbArrN
      where
        jsonFlds = intercalate' ", "
          $ (qplains <&> \qp -> let fld = fromText qp.fpDbName in
            "'" <> fld <> "', " <> fld <> sn)
          <> (arrs <&> \(dbn, arr) -> "'" <> dbn <> "', to_jsonb(" <> arr <> ")")
  tell (mempty, fmap (spaces <>) $ appendArray <> [endLoop] )
  pure mbArrN
  where
    (iplains, ichildren) = P.foldr (\case
      DmlFieldPlain ifp -> first (ifp :)
      DmlFieldTo ift -> second (ift:)) mempty ir.iFields
    (qplains, qchildren) = P.foldr (\case
      QFieldPlain qfp -> first (qfp:)
      QFieldTo qft -> second (qft:)
      _ -> id) mempty qfs
