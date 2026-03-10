{-# LANGUAGE OverloadedStrings #-}
module Main where

import PgSchema.Generation


main :: IO Bool
main = updateSchemaFile False "pg-schema-tutorial/app/Sch.hs"
  (Right "dbname=schema_test user=avia host=localhost")
  "Sch" -- ^ haskell module name to generate
  "Sch" -- ^ name of generated haskell type for schema
  (GenNames ["sch"] [] []) -- ^ name of schemas in database
