{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import PgSchema.Generation

main :: IO Bool
main = updateSchemaFile False "tutorial/app/app/Sch.hs"
  (Right "dbname=tutorial")
  "Sch" -- ^ haskell module name to generate
  "Sch" -- ^ name of generated haskell type for schema
  (GenNames ["tut"] [] []) -- ^ name of schemas in database
