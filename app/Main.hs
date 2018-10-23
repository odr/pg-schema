module Main where

import Database.PostgreSQL.Schema.TH


data Sch

mkSchema "dbname=tinkoff_development user=avia host=localhost" ''Sch "tinkoff"

main :: IO ()
main = return ()
