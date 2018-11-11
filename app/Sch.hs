module Sch where

import Database.PostgreSQL.Schema.TH


data Sch

mkSchema "dbname=schema_test host=localhost user=postgres" ''Sch "sch"
