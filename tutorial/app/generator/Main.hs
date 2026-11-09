{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import PgSchema.Generation

main :: IO Bool
main = updateSchemaFile False "tutorial/app/app/Sch.hs"
  (Left "PG_CONN") -- ^ connection string from the PG_CONN environment variable
  "Sch" -- ^ haskell module name to generate
  "Sch" -- ^ name of generated haskell type for schema
  (GenNames ["tut"] [] []) -- ^ name of schemas in database
