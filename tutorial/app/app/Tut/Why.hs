{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Введение (index.md): минимальный пример. Только компилируется
-- (в главе нет вывода), чтобы пример в тексте не разошёлся с библиотекой.
module Tut.Why (whyExample, whyParam) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
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

-- --8<-- [start:why-param]
data Project task = Project
  { title                 :: Text
  , tasks_project_id_fkey :: [task]
  } deriving (Show, Generic)

data TaskTitle = TaskTitle { title :: Text }
  deriving (Show, Generic)

data TaskCard = TaskCard { title :: Text, priority :: Int32, watchers :: PgArr Int64 }
  deriving (Show, Generic)

whyParam :: Connection -> IO ([Project TaskTitle], [Project TaskCard])
whyParam conn = do
  (menu, _)  <- selectSch (MyAnn "projects") conn qpEmpty -- только названия задач
  (cards, _) <- selectSch (MyAnn "projects") conn qpEmpty -- задачи для карточек
  pure (menu, cards)
-- --8<-- [end:why-param]
