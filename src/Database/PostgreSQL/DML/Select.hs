-- {-# LANGUAGE CPP #-}
module Database.PostgreSQL.DML.Select where

-- import Data.Foldable as FD
-- import Data.Semigroup ((<>))
-- import Data.Text as T
-- import Database.PostgreSQL.Rec
-- import Database.Schema.Util.ToStar

-- selRec :: forall sch tab rec. CRecDef sch tab rec => Text
-- selRec = "select "
--   <> T.intercalate "," (friPgName <$> toStar @_ @(RecFldInfos sch tab rec))
--   <> " from " <> toStar @_ @tab
--
-- jsonObj :: forall sch tab rec. CRecDef sch tab rec => Text
-- jsonObj = "jsonb_build_object("
--   <> T.intercalate ","
--     ( (\a -> "'" <> a <> "'," <> a) . friPgName
--       <$> toStar @_ @(RecFldInfos sch tab rec) )
--   <> ")"
--
-- selText :: forall sch tab rec. CRecDef sch tab rec => Text
-- selText =
--   sel
--     (toStar @_ @(RecFldInfos sch tab rec))
--     (toStar @_ @(RecToInfos sch tab rec))
--     (toStar @_ @(RecFromInfos sch tab rec))
--   where
--     sel flds tos froms =

{-
select rel.relname, rel.relkind
  , array_to_json(array(
    select
      jsonb_build_object(
        'name', c.attname
        , 'num', c.attnum
        , 'null', not c.attnotnull
        , 'typ', json_build_object(
          'name', t.typname
          , 'typns', tns.nspname
          , 'typ', t.typtype
          , 'cat', t.typcategory
          )
      )
      from pg_attribute c
        join pg_type t on c.atttypid = t.oid
        left outer join pg_namespace tns on t.typnamespace = tns.oid
      where c.attrelid = rel.oid
        and c.attnum > 0
      order by c.attnum
    )) cols
    , array_to_json(array(
      select
        jsonb_build_object(
          'typ', con.contype
          , 'key', array_to_json(con.conkey)
        )
        from pg_constraint con
        where con.contype in ('p','u')
          and con.conrelid = rel.oid))
  from pg_catalog.pg_class rel
    join pg_catalog.pg_namespace nsp on rel.relnamespace = nsp.oid
  where nsp.nspname = 'tinkoff'
        and rel.relkind in ('r','v')
-}
