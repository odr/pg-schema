module Sch where

import Database.PostgreSQL.Schema.TH


hashSchema :: Int
hashSchema = 0

data Sch

mkSchema "dbname=schema_test user=postgres" ''Sch "sch"
