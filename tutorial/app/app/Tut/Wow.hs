-- --8<-- [start:extensions]
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
-- --8<-- [end:extensions]
-- | Глава «Быстрый старт»: вставка и чтение дерева «проект → задачи».
module Tut.Wow (wow) where

-- --8<-- [start:imports]
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import PgSchema.DML
import Sch
-- --8<-- [end:imports]
import Tut.Output

-- --8<-- [start:ann]
type TutAnn tab = 'Ann RenamerId Sch 3 ("tut" ->> tab)
-- --8<-- [end:ann]

-- --8<-- [start:types]
data Task = Task
  { seq      :: Int32
  , title    :: Text
  , priority :: Int32
  } deriving (Show, Generic)

data Project = Project
  { owner_id              :: Int64
  , title                 :: Text
  , status                :: PGEnum Sch ("tut" ->> "project_status")
  , tasks_project_id_fkey :: [Task]
  } deriving (Show, Generic)
-- --8<-- [end:types]

-- --8<-- [start:key-types]
data TaskKey = TaskKey
  { project_id :: Int64
  , seq        :: Int32
  } deriving (Show, Generic)

data ProjectKey = ProjectKey
  { id                    :: Int64
  , tasks_project_id_fkey :: [TaskKey]
  } deriving (Show, Generic)
-- --8<-- [end:key-types]

-- --8<-- [start:owner-type]
newtype Owner = Owner { name :: Text } deriving (Show, Generic)
-- --8<-- [end:owner-type]

-- --8<-- [start:query]
activeProjects :: QueryParamAnn (TutAnn "projects")
activeProjects = qRoot do
  qWhere $ "status" =? Project_status_active
  qPath "tasks_project_id_fkey" do
    qWhere $ "priority" <? (50 :: Int32)
    qOrderBy [ascf "seq"]
-- --8<-- [end:query]

wow :: Connection -> IO ()
wow conn = do
  -- --8<-- [start:wow-owner]
  _ <- insertSch_ (TutAnn "users") conn [Owner "Alice"]
  -- --8<-- [end:wow-owner]

  sqlIns <- section "wow-insert" $ do
    -- --8<-- [start:wow-insert]
    let
      projects =
        [ Project 1 "Website" Project_status_active
          [ Task 1 "Design" 10
          , Task 2 "Backend" 30
          , Task 3 "Nice to have" 100 ]
        , Project 1 "Mobile app" Project_status_draft
          [ Task 1 "Research" 20 ] ]
    (keys :: [ProjectKey], sqlIns) <- insertJSON (TutAnn "projects") conn projects
    mapM_ print keys
    -- --8<-- [end:wow-insert]
    pure sqlIns

  section "wow-insert-sql" $ putStrLn (T.unpack sqlIns)

  section "wow-select" $ do
    -- --8<-- [start:wow-select]
    (rows :: [Project], (sql, params)) <- selectSch (TutAnn "projects") conn activeProjects
    mapM_ print rows
    putStrLn $ T.unpack sql
    print params
    -- --8<-- [end:wow-select]

  section "wow-select-text" $ do
    -- --8<-- [start:wow-select-text]
    let (sql, params) = selectText (TutAnn "projects") @Project activeProjects
    putStrLn $ T.unpack sql
    print params
    -- --8<-- [end:wow-select-text]
