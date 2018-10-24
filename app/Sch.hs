module Sch where

import Database.PostgreSQL.Schema.TH


data Sch

mkSchema "dbname=schema_test user=avia host=localhost" ''Sch "sch"
