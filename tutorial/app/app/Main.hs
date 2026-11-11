{-# LANGUAGE OverloadedStrings #-}
-- | Runs the examples of all chapters in order.
--
-- The schema is recreated before each independent chapter ('resetDb'): ids in
-- the output depend on it. Chapters 02 and 03 share their data. Code and output
-- get into the docs via pymdownx.snippets, see @tutorial/README.md@.
-- Run from the repository root (the path to the SQL script is relative).
module Main (main) where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as BS
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute_)
import Database.PostgreSQL.Simple.Types (Query (..))
import System.Environment (lookupEnv)
import Tut.Basics (basics)
import Tut.FlatDml (flatDml)
import Tut.Practice (practice)
import Tut.Select (select)
import Tut.Output (section, sqlSection)
import Tut.Why (whyExample)
import Tut.Types (types)
import Tut.TreeDml (treeDml)
import Tut.Wow (wow)

main :: IO ()
main = do
  connStr <- maybe "dbname=tutorial" BS.pack <$> lookupEnv "PG_CONN"
  conn <- connectPostgreSQL connStr
  resetDb conn
  wow conn         -- 01-wow
  resetDb conn
  basics conn      -- 02-core-workflow
  flatDml conn     -- 03-flat-dml
  resetDb conn
  select conn      -- 04-select
  sqlWhy <- section "why-select" (whyExample conn) -- index (on the chapter 04 data)
  sqlSection "why-select" [sqlWhy]
  types conn       -- appendix-types (on the chapter 04 data)
  resetDb conn
  treeDml conn     -- 05-tree-dml
  resetDb conn
  practice conn    -- 07-practice

-- | Recreate the @tut@ schema (the script starts with @drop schema if exists tut cascade@).
resetDb :: Connection -> IO ()
resetDb conn = do
  sql <- BS.readFile "tutorial/sql/00-create-schema.sql"
  void $ execute_ conn (Query sql)
