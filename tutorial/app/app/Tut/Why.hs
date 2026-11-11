{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Introduction (index.md): a minimal example and a parameterized record.
-- Runs on the chapter 04 data (after its examples).
module Tut.Why (whyExample, whyParam) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import PgSchema.DML
import Sch

-- --8<-- [start:why-ann]
type MyAnn tab = 'Ann RenamerId Sch 5 ("tut" ->> tab)
-- --8<-- [end:why-ann]

whyExample :: Connection -> IO Text
whyExample conn = do
  -- --8<-- [start:why-select]
  (rows :: ["title" := Text :. "tasks_project_id_fkey" := ["title" := Text :. "priority" := Int32]], (sql, params)) <-
    selectSch (MyAnn "projects") conn $ qRoot do
      qWhere $ "status" =? Project_status_active
      qPath "tasks_project_id_fkey" do
        qWhere $ "priority" <? (50 :: Int32)
  -- --8<-- [end:why-select]
  mapM_ print rows
  print params
  pure sql

-- --8<-- [start:why-param]
data Context = List | Details

data Task = Task { title :: Text, priority :: Int32 }
  deriving (Show, Generic)

data Project (c :: Context) = Project
  { title                  :: Text
  , projects_owner_id_fkey :: If (c == 'List) () ("name" := Text)
  , tasks_project_id_fkey  :: If (c == 'List) () [Task]
  } deriving Generic

whyParam :: Connection -> IO ([Project 'List], [Project 'Details])
whyParam conn = do
  (list, _)    <- selectSch (MyAnn "projects") conn qpEmpty -- titles only
  (details, _) <- selectSch (MyAnn "projects") conn qpEmpty -- with the owner and tasks
  pure (list, details)
-- --8<-- [end:why-param]

deriving instance Show (Project 'List)
deriving instance Show (Project 'Details)
