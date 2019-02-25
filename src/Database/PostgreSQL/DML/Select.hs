{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE CPP #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS (MonadRWS, ask, evalRWS, get, local, modify)
import Data.Bifunctor
import Data.List as L
import Data.Semigroup ((<>))
import Data.String
import Data.Text as T
import Data.Text.Lazy (toStrict)
import Database.PostgreSQL.DB
import Database.PostgreSQL.DML.Condition
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec
import Formatting

data QueryRead = QueryRead
  { qrSchema     :: Text
  , qrCurrTabNum :: Int
  , qrIsRoot     :: Bool }
  deriving Show

data QueryState = QueryState
  { qsLastTabNum :: Int
  , qsJoins      :: [Text] }
  deriving Show

type MonadQuery m = MonadRWS QueryRead () QueryState m

selectSch
  :: forall sch tab r. (FromRow r, CQueryRecord PG sch tab r)
  => Connection -> Cond sch tab -> IO [r]
selectSch conn cond = let (q,c) = selectQuery @sch @tab @r cond in
  query conn q c

selectQuery
  :: forall sch tab r. CQueryRecord PG sch tab r
  => Cond sch tab -> (Query,[SomeToField])
selectQuery = first (fromString . unpack) . selectText @sch @tab @r

selectText
  :: forall sch tab r. CQueryRecord PG sch tab r
  => Cond sch tab -> (Text,[SomeToField])
selectText cond
  | condText == mempty  = (sel, [])
  | otherwise           = (sel <> " where " <> toStrict condText, pars)
  where
    sel = fst $ evalRWS (selectM (getQueryRecord @PG @sch @tab @r))
      (QueryRead (schemaName @sch) 0 True) (QueryState 0 [])
    (condText, pars) = pgCond cond

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

selectM :: MonadQuery m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  QueryRead {..} <- ask --(schName,(n,isRoot))
  fields <- traverse fieldM queryFields
  joins <- L.reverse . qsJoins <$> get
  let
    sel
      | qrIsRoot    =
        T.intercalate "," $ L.map (\(a,b) -> a <> " \"" <> b <> "\"") fields
      | otherwise = jsonPairing fields
    j = T.intercalate " " joins
  pure $ sformat fmt sel qrSchema tableName qrCurrTabNum j
  where
    fmt = "select " % stext
      % " from " % stext % "." % stext % " t" % int % " " % stext

fieldM :: MonadQuery m => QueryField -> m (Text, Text)
fieldM (FieldPlain name dbname _) = do
  n <- qrCurrTabNum <$> ask
  pure $ (sformat fmt n dbname, name)
  where
    fmt = "t" % int % "." % stext

fieldM (FieldFrom name QueryRecord {..} refs) = do
  QueryRead {..} <- ask -- (schName,(n1,_))
  modify (\QueryState{..} -> QueryState
    (qsLastTabNum+1)
    (joinText qrSchema qrCurrTabNum (qsLastTabNum+1) : qsJoins))
  n2 <- qsLastTabNum <$> get
  f <- jsonPairing
    <$> local (\qr -> qr { qrCurrTabNum = n2, qrIsRoot = False })
      (traverse fieldM queryFields)
  pure (f, name)
  where
    joinText schName n1 n2 =
      sformat fmt outer schName tableName n2 (refCond n1 n2 refs)
      where
        fmt = stext % "join " % stext % "." % stext % " t" % int
          % " on " % stext
        outer
          | L.any (fdNullable . fromDef) refs = "left outer "
          | otherwise = ""

fieldM (FieldTo name rec refs) = do
  n1 <- qrCurrTabNum <$> ask
  (QueryState ltn joins) <- get
  modify (const $ QueryState (ltn+1) [])
  selText <- local
    (\qr -> qr { qrCurrTabNum = ltn+1, qrIsRoot = False})
    (selectM rec)
  modify (\qs -> qs { qsJoins = joins })
  pure (sformat fmt selText (refCond (ltn+1) n1 refs), name)
  where
    fmt = "array_to_json(array(" % stext % " where " % stext % "))"

refCond :: Int -> Int -> [QueryRef] -> Text
refCond nFrom nTo = T.intercalate " and " . L.map compFlds
  where
    fld = "t" % int % "." % stext
    compFlds QueryRef {..} = sformat (fld % "=" % fld) nFrom fromName nTo toName

--
-- {-
-- fldExp = case toStar @_ @fldRecInfoSum
--
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
--   where nsp.nspname = 'tinkoff'
--         and rel.relkind in ('r','v')
-- "
-- -}
