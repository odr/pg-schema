{-# LANGUAGE OverloadedStrings #-}
module Main where

import PgSchema


main = updateSchemaFile' False
  "pg-schema-tutorial/app/Sch.hs"
  "dbname=schema_test user=avia host=localhost"
  "Sch" -- ^ haskell module name to generate
  "Sch" -- ^ name of generated haskell type for schema
  (GenNames ["sch"] [] []) -- ^ name of schemas in database
