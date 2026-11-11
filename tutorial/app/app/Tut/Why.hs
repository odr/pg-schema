{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
-- | Введение (index.md): минимальный пример. Только компилируется
-- (в главе нет вывода), чтобы пример в тексте не разошёлся с библиотекой.
module Tut.Why (whyExample) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import PgSchema.DML
import Sch

-- --8<-- [start:why-ann]
type MyAnn tab = 'Ann RenamerId Sch 5 ("tut" ->> tab)
-- --8<-- [end:why-ann]

whyExample :: Connection -> IO ()
whyExample conn = do
  -- --8<-- [start:why-select]
  (rows :: ["title" := Text :. "tasks_project_id_fkey" := ["title" := Text :. "priority" := Int32]], (sql, params)) <-
    selectSch (MyAnn "projects") conn $ qRoot do
      qWhere $ "status" =? Project_status_active
      qPath "tasks_project_id_fkey" do
        qWhere $ "priority" <? (50 :: Int32)
  -- --8<-- [end:why-select]
  print (rows, sql, params)
