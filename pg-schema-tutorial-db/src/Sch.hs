module Sch where

import Database.PostgreSQL.Schema.TH


data Sch

mkSchema "dbname=schema_test user=postgres" ''Sch "sch"

hashSchema :: Int
-- next line is generated from Setup.hs.
-- It should be in form 'hashSchema = <intValue>'
hashSchema = -7695617572686231947
