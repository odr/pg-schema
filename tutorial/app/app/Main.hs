{-# LANGUAGE OverloadedStrings #-}
-- | Запускает примеры всех глав по порядку.
--
-- Перед каждой независимой главой схема пересоздаётся ('resetDb'): от этого
-- зависят id в выводе. Главы 02 и 03 работают с одними данными. Код и вывод
-- попадают в документацию через pymdownx.snippets, см. @tutorial/README.md@.
-- Запускается из корня репозитория (путь к SQL-скрипту относительный).
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
  resetDb conn
  treeDml conn     -- 05-tree-dml
  resetDb conn
  practice conn    -- 07-practice

-- | Пересоздать схему @tut@ (скрипт начинается с @drop schema if exists tut cascade@).
resetDb :: Connection -> IO ()
resetDb conn = do
  sql <- BS.readFile "tutorial/sql/00-create-schema.sql"
  void $ execute_ conn (Query sql)
