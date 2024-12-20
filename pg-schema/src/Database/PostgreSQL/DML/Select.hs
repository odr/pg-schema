{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS
import Control.Monad
import Data.Bifunctor
import Data.List as L
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple

import Database.PostgreSQL.DB
import Database.PostgreSQL.DML.Condition
import Database.PostgreSQL.DML.Limit
import Database.PostgreSQL.DML.Order
import Database.Schema.Def
import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util


data QueryParam sch t = QueryParam
  { qpConds :: ![CondWithPath sch t]
  , qpOrds  :: ![OrdWithPath sch t]
  , qpLOs   :: ![LimOffWithPath sch t] }

qpEmpty :: forall sch t. QueryParam sch t
qpEmpty = QueryParam [] [] []

data QueryRead sch t = QueryRead
  { qrCurrTabNum :: !Int
  , qrIsRoot     :: !Bool
  , qrPath       :: ![Text]
  , qrParam      :: !(QueryParam sch t) }

data QueryState = QueryState
  { qsLastTabNum :: !Int
  , qsJoins      :: ![Text]
  , qsHasWhere   :: !Bool
  , qsOrd        :: !Text
  , qsLimOff     :: !Text }
  deriving Show

type MonadQuery sch t m =
  (CSchema sch, MonadRWS (QueryRead sch t) [SomeToField] QueryState m)

selectSch
  :: forall sch tab r. (FromRow r, CQueryRecord PG sch tab r)
  => Connection -> QueryParam sch tab -> IO [r]
selectSch conn cond = let (q,c) = selectQuery @sch @tab @r cond in
  query conn q c

selectQuery
  :: forall sch tab r. CQueryRecord PG sch tab r
  => QueryParam sch tab -> (Query,[SomeToField])
selectQuery = first (fromString . unpack) . selectText @sch @tab @r

selectText
  :: forall sch tab r. CQueryRecord PG sch tab r
  => QueryParam sch tab -> (Text,[SomeToField])
selectText qp = evalRWS (selectM (getQueryRecord @PG @sch @tab @r)) (qr0 qp) qs0

qr0 :: QueryParam sch t -> QueryRead sch t
qr0 qrParam = QueryRead
    { qrCurrTabNum = 0
    , qrIsRoot = True
    , qrPath = []
    , qrParam }

qs0 :: QueryState
qs0 = QueryState
    { qsLastTabNum = 0
    , qsJoins = []
    , qsHasWhere = False
    , qsOrd = ""
    , qsLimOff = "" }

two :: (a,b,c) -> (a,b)
two (a,b,_c) = (a,b)

third :: (a,b,c) -> c
third (_a,_b,c) = c

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

selectM :: MonadQuery sch t m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  QueryRead {..} <- ask
  fields <- traverse fieldM queryFields
  joins <- gets $ L.reverse . qsJoins
  let
    (condText, pars) =
      condByPath qrCurrTabNum (L.reverse qrPath) $ qpConds qrParam
    sel
      | qrIsRoot    =
        T.intercalate "," $ L.map (\(a,b,_) -> a <> " \"" <> b <> "\"") fields
      | otherwise = jsonPairing $ two <$> fields
    whereText
      | condText == mempty = ""
      | otherwise       = " where " <> condText
    qsOrd
      | t == mempty = ""
      | otherwise = " order by " <> t
      where
        t = ordByPath qrCurrTabNum (L.reverse qrPath) $ qpOrds qrParam
    qsLimOff = loByPath (L.reverse qrPath) $ qpLOs qrParam
  modify (\qs -> qs { qsHasWhere = whereText /= mempty })
  unless qrIsRoot $ modify (\qs -> qs { qsOrd, qsLimOff })
  tell pars
  pure $ "select " <> sel
    <> " from " <> qualName tableName <> " t" <> show' qrCurrTabNum
    <> " " <> T.unwords joins
    <> whereText
    <> (if qrIsRoot then qsOrd else "")
    <> (if qrIsRoot then qsLimOff else "")

-- | return text for field, alias and expression to check is empty
-- (not obvious for FieldTo)
fieldM :: MonadQuery sch tab m => QueryField -> m (Text, Text, Text)
fieldM (FieldPlain _ dbname _) = do
  n <- asks qrCurrTabNum
  let val = "t" <> show' n <> "." <> dbname
  pure (val, dbname, val <> " is null")

fieldM (FieldFrom _ dbname QueryRecord{..} refs) = do
  QueryRead {..} <- ask
  modify \QueryState{qsLastTabNum, qsJoins} -> QueryState
    { qsLastTabNum = qsLastTabNum+1
    , qsJoins = joinText qrCurrTabNum (qsLastTabNum+1) : qsJoins
    , qsHasWhere = False
    , qsOrd = ""
    , qsLimOff = "" }
  n2 <- gets qsLastTabNum
  flds <- local
    (\qr -> qr{ qrCurrTabNum = n2, qrIsRoot = False, qrPath = dbname : qrPath })
    $ traverse fieldM queryFields
  let val = fldt flds
  pure (val, dbname, val <> " is null")
  where
    nullable = L.any (fdNullable . fromDef) refs
    joinText n1 n2 =
      outer <> "join " <> qualName tableName <> " t" <> show' n2
          <> " on " <> refCond n1 n2 refs
      where
        outer
          | nullable = "left outer "
          | otherwise = ""
    fldt flds
      | nullable = "case when " <> isNull <>
          " then null else " <> jsonPairing (two <$> flds) <> " end"
      | otherwise = jsonPairing $ two <$> flds
      where
        isNull = T.intercalate " and " $ third <$> flds

fieldM (FieldTo _ dbname rec refs) = do
  QueryRead{..} <- ask
  QueryState {qsLastTabNum, qsJoins} <- get
  modify (const $ QueryState (qsLastTabNum+1) [] False "" "")
  selText <- local
    (\qr -> qr
      { qrCurrTabNum = qsLastTabNum+1, qrIsRoot = False, qrPath = dbname : qrPath })
    (selectM rec)
  modify (\qs -> qs { qsJoins = qsJoins })
  QueryState{qsHasWhere, qsOrd, qsLimOff} <- get
  let
    val = "array(" <> selText <> " " <> (if qsHasWhere then "and" else "where")
      <> " " <> refCond (qsLastTabNum+1) qrCurrTabNum refs
      <> qsOrd <> qsLimOff <> ")"
  pure ("array_to_json(" <> val <> ")", dbname, val <> " = '{}'")

refCond :: Int -> Int -> [QueryRef] -> Text
refCond nFrom nTo = T.intercalate " and " . fmap compFlds
  where
    compFlds QueryRef {fromName, toName} =
      fldt nFrom fromName <> "=" <> fldt nTo toName
      where
        fldt n = (("t" <> show' n <> ".") <>)


--
-- {-
-- "
-- select rel.relname, rel.relkind
--   , array_to_json(array(
--     select
--       jsonb_build_object(
--         'name', c.attname
--         , 'num', c.attnum
--         , 'null', not c.attnotnull
--         , 'typ', json_build_object(
--           'name', t.typname
--           , 'typns', tns.nspname
--           , 'typ', t.typtype
--           , 'cat', t.typcategory
--           )
--       )
--       from pg_attribute c
--         join pg_type t on c.atttypid = t.oid
--         left outer join pg_namespace tns on t.typnamespace = tns.oid
--       where c.attrelid = rel.oid
--         and c.attnum > 0
--       order by c.attnum
--     )) cols
--     , array_to_json(array(
--       select
--         jsonb_build_object(
--           'typ', con.contype
--           , 'key', array_to_json(con.conkey)
--         )
--         from pg_constraint con
--         where con.contype in ('p','u')
--           and con.conrelid = rel.oid))
--   from pg_catalog.pg_class rel
--     join pg_catalog.pg_namespace nsp on rel.relnamespace = nsp.oid
--   where nsp.nspname = 'test'
--         and rel.relkind in ('r','v')
-- "
-- -}
