{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Schema.Schema
import Debug.Trace
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo


main = do
  updateSchemaHash "dbname=schema_test user=postgres" "sch" "app/Sch.hs"
  defaultMain
