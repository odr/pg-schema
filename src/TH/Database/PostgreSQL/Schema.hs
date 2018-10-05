module TH.Database.PostgreSQL.Schema where

import Database.PostgreSQL.Simple
import Language.Haskell.TH

mkSchema :: DecsQ
mkSchema = do
  conn <- runIO
    $ connectPostgreSQL "dbname=tinkoff_development user=avia host=localhost"

  undefined conn

{-
select t.typname, tns.nspname
    , array(select enumlabel from pg_enum e where e.enumtypid = t.oid)
  from pg_type t
    left outer join pg_namespace tns on t.typnamespace = tns.oid
  where t.typtype='e'

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

select con.conname,rel.relname, frel.relname, con.conkey, con.confkey, con.confdeltype, con.confupdtype
  from pg_constraint con
    join pg_namespace nsp on con.connamespace = nsp.oid
    join pg_class rel on con.conrelid = rel.oid
    join pg_class frel on con.confrelid = frel.oid
  where nsp.nspname = 'tinkoff'
    and con.contype = 'f'


-}
