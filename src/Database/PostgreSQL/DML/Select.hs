{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE CPP #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS.Class
import Data.List as L
import Data.Semigroup ((<>))
import Data.Text as T
import Database.Schema.Def
import Database.Schema.Rec
import Formatting


type MonadQuery m = MonadRWS Int Text Int m

selectM :: MonadQuery m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  n <- ask
  fields <- L.map selectPair <$> traverse fieldM queryFields
  pure $ sformat fmt (T.intercalate "," fields) tableName n
  where
    selectPair (a,b) = a <> " " <> b
    fmt = "select " % stext % " from " % stext % " t" % int

fieldM :: MonadQuery m => QueryField -> m (Text, Text)
fieldM (FieldPlain name dbname _) = do
  n <- ask
  pure $ (sformat fmt n dbname, name)
  where
    fmt = "t" % int % "." % stext

fieldM (FieldFrom name QueryRecord {..} refs) = do
  n1 <- ask
  modify (+1)
  n2 <- get
  tell $ join n1 n2
  jsonList <- T.intercalate "," . L.map jsonPair <$> traverse fieldM queryFields
  pure ("jsonb_build_object(" <> jsonList <> ")", name)
  where
    jsonPair (a,b) = "'" <> b <> "'," <> a
    join n1 n2 = sformat fmt outer tableName n2 (refCond n1 n2 refs)
      where
        fmt = stext % "join " % stext % " t" % int % " on " % stext
        outer
          | L.any (fdNullable . fromDef) refs = "left outer "
          | otherwise = ""

fieldM (FieldTo name rec refs) = do
  n1 <- ask
  modify (+1)
  n2 <- get
  selectText <- local (const n2) $ selectM rec
  pure (sformat fmt selectText (refCond n2 n1 refs), name)
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
