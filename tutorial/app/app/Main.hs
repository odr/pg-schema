{-# LANGUAGE OverloadedStrings #-}
-- | Запускает примеры всех глав по порядку.
--
-- Примеры рассчитаны на свежую БД (@make tut-db-init@): от этого зависят
-- id в выводе. Код и вывод попадают в документацию через pymdownx.snippets,
-- см. @tutorial/README.md@.
module Main (main) where

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Tut.Basics (basics)
import Tut.FlatDml (flatDml)
import Tut.TreeDml (treeCompare, treeDml)

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=tutorial"
  basics conn      -- 02-core-workflow
  flatDml conn     -- 03-dml-operations: плоские операции
  treeDml conn     -- 03-dml-operations: операции со связанными таблицами
  treeCompare conn -- 03-dml-operations: сравнение insert / upsert / update
