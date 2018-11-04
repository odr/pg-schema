module Sch where

import Database.PostgreSQL.Schema.TH


data Sch

mkSchema "dbname=schema_test user=test host=localhost" ''Sch "sch"
