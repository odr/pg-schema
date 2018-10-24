{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE CPP #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS (MonadRWS, ask, evalRWS, get, local, modify)
import Data.Bifunctor
import Data.List as L
import Data.Semigroup ((<>))
import Data.String
import Data.Text as T
import Database.PostgreSQL.DB
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec
import Formatting


type MonadQuery m = MonadRWS (Text,(Int,Bool)) () (Int,[Text]) m

selectSch_
  :: forall sch tab r. (FromRow r, CQueryRecord PG sch tab r)
  => Connection -> IO [r]
selectSch_ conn = query_ conn (selectQuery @sch @tab @r)

selectQuery :: forall sch tab r. CQueryRecord PG sch tab r => Query
selectQuery = fromString $ unpack (selectText @sch @tab @r)

selectText :: forall sch tab r. CQueryRecord PG sch tab r => Text
selectText = fst
  $ evalRWS (selectM (getQueryRecord @PG @sch @tab @r))
    (schemaName @sch,(0,True)) (0,[])

-- jsonPairing :: [(Text, Text)] -> Text
-- jsonPairing fs = case fs of
--   [(a,_)] -> a
--   _       -> "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
--   where
--     pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

selectM :: MonadQuery m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  (schName,(n,isRoot)) <- ask
  fields <- traverse fieldM queryFields
  (_,joins) <- second L.reverse <$> get
  let
    sel
      | isRoot    =
        T.intercalate "," $ L.map (\(a,b) -> a <> " \"" <> b <> "\"") fields
      | otherwise = jsonPairing fields
    j = T.intercalate " " joins
  pure $ sformat fmt sel schName tableName n j
  where
    fmt = "select " % stext
      % " from " % stext % "." % stext % " t" % int % " " % stext

fieldM :: MonadQuery m => QueryField -> m (Text, Text)
fieldM (FieldPlain name dbname _) = do
  (n,_) <- snd <$> ask
  pure $ (sformat fmt n dbname, name)
  where
    fmt = "t" % int % "." % stext

fieldM (FieldFrom name QueryRecord {..} refs) = do
  (schName,(n1,_)) <- ask
  modify (\(n2,joins) -> (n2+1, joinText schName n1 (n2+1) : joins))
  (n2,_) <- get
  f <- jsonPairing
    <$> local (second $ const (n2,False)) (traverse fieldM queryFields)
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
  (n1,_) <- snd <$> ask
  (n2, joins) <- get
  modify (const (n2+1,[]))
  selText <- local (second $ const $ (n2+1,False)) $ selectM rec
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
