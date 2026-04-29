```haskell
module Sch where

import PgSchema.Import

data Sch

instance CSchema Sch where
  type TTabs Sch = '[ ( "tut" ->> "projects" ),( "tut" ->> "task_events" )
    ,( "tut" ->> "tasks" ),( "tut" ->> "users" ) ]

  type TTypes Sch = '[ ( "pg_catalog" ->> "_int8" )
    ,( "pg_catalog" ->> "_text" ),( "pg_catalog" ->> "float8" )
    ,( "pg_catalog" ->> "int4" ),( "pg_catalog" ->> "int8" )
    ,( "pg_catalog" ->> "jsonb" ),( "pg_catalog" ->> "text" )
    ,( "pg_catalog" ->> "timestamptz" ),( "tut" ->> "project_status" ) ]
```
