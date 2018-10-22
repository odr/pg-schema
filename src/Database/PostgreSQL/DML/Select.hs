{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE CPP #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS (MonadRWS, ask, evalRWS, get, local, modify)
import Data.Bifunctor
import Data.List as L
import Data.Semigroup ((<>))
import Data.Text as T
import Database.PostgreSQL.DB
import Database.Schema.Def
import Database.Schema.Rec
import Formatting


type MonadQuery m = MonadRWS (Int,Bool) () (Int,[Text]) m

selectText :: forall sch tab r. CQueryRecord PG sch tab r => Text
selectText = fst
  $ evalRWS (selectM (getQueryRecord @PG @sch @tab @r)) (0,True) (0,[])

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

selectM :: MonadQuery m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  (n,isRoot) <- ask
  fields <- traverse fieldM queryFields
  (_,joins) <- get
  let
    sel
      | isRoot    = T.intercalate "," $ L.map (\(a,b) -> a <> " " <> b) fields
      | otherwise = jsonPairing fields
    j = T.intercalate " " joins
  pure $ sformat fmt sel tableName n j
  where
    fmt = "select " % stext % " from " % stext % " t" % int % " " % stext

fieldM :: MonadQuery m => QueryField -> m (Text, Text)
fieldM (FieldPlain name dbname _) = do
  (n,_) <- ask
  pure $ (sformat fmt n dbname, name)
  where
    fmt = "t" % int % "." % stext

fieldM (FieldFrom name QueryRecord {..} refs) = do
  (n1,_) <- ask
  modify (\(n2,joins) -> (n2+1, joinText n1 (n2+1) : joins))
  (n2,_) <- get
  f <- jsonPairing <$> local (const (n2,False)) (traverse fieldM queryFields)
  pure (f, name)
  where
    joinText n1 n2 = sformat fmt outer tableName n2 (refCond n1 n2 refs)
      where
        fmt = stext % "join " % stext % " t" % int % " on " % stext
        outer
          | L.any (fdNullable . fromDef) refs = "left outer "
          | otherwise = ""

fieldM (FieldTo name rec refs) = do
  (n1,_) <- ask
  (n2, joins) <- get
  modify (const (n2+1,[]))
  selText <- local (const $ (n2+1,False)) $ selectM rec
  modify (second $ const joins)
  pure (sformat fmt selText (refCond (n2+1) n1 refs), name)
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
