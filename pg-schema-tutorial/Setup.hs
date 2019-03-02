{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Database.PostgreSQL.Schema.Schema
import Debug.Trace
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo


main = do
  void $ updateSchemaHash "dbname=schema_test user=postgres" "sch"
    "../pg-schema-tutorial-db/src/Sch.hs"
  defaultMain
