{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (when)
import PgSchema.Generation
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- --8<-- [start:gen-names]
genNames :: GenNames
genNames = GenNames
  { schemas = ["tut", "audit"] -- all tables of these schemas
  , tables = []                -- and single tables of other schemas (none here)
  , addRelations =             -- relations that are missing in the DB
    [ AddRelation
      { name = "task_events_author"
      , from = "tut" ->> "task_events"
      , to = "tut" ->> "users"
      , cols = [("author_id", "id")] } ] }
-- --8<-- [end:gen-names]

-- --8<-- [start:gen-main]
main :: IO ()
main = do
  check <- elem "--check" <$> getArgs
  changed <- updateSchemaFile False "tutorial/app/app/Sch.hs"
    (Left "PG_CONN") -- connection string from the PG_CONN environment variable
    "Sch" -- name of the generated module
    "Sch" -- name of the schema type
    genNames
  when (check && changed) $ do
    hPutStrLn stderr "Sch.hs did not match the database and has been regenerated"
    exitFailure
-- --8<-- [end:gen-main]
