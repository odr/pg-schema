{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Глава «Select»: формы результата, условия, связи, сортировка, агрегаты.
module Tut.Select (select) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import GHC.Generics (Generic)
import PgSchema.DML
import Sch
import Tut.Common
import Tut.Output

-- --8<-- [start:sel-payload]
newtype Payload = Payload { estimate :: Int } deriving (Show, Generic)

instance FromJSON Payload
instance ToJSON Payload

instance FromField Payload where fromField = fromJSONField
instance ToField Payload where toField = toJSONField
-- --8<-- [end:sel-payload]

-- --8<-- [start:sel-seed-types]
type UserIn = "id" := Int64 :. "name" := Text :. "email" := Maybe Text
  :. "managerId" := Maybe Int64

type ProjectIn = "id" := Int64 :. "ownerId" := Int64 :. "title" := Text
  :. "status" := ProjectStatus :. "tags" := PgArr Text

type TaskIn = "projectId" := Int64 :. "seq" := Int32 :. "title" := Text
  :. "priority" := Int32 :. "payload" := Maybe Payload :. "watchers" := PgArr Int64
-- --8<-- [end:sel-seed-types]

seed :: Connection -> IO ()
seed conn = do
  -- --8<-- [start:sel-seed]
  _ <- insertSch_ (MyAnn "users") conn $ rowsOf @UserIn
    [ 1 :. "Alice" :. Just "alice@example.com" :. Nothing
    , 2 :. "Bob"   :. Just "bob@example.com"   :. Just 1
    , 3 :. "Carol" :. Nothing                  :. Just 1
    , 4 :. "Dave"  :. Just "dave@example.com"  :. Just 2 ]
  _ <- insertSch_ (MyAnn "projects") conn $ rowsOf @ProjectIn
    [ 1 :. 1 :. "Website" :. Project_status_active   :. pgArr' ["web", "frontend"]
    , 2 :. 2 :. "API"     :. Project_status_active   :. pgArr' ["backend"]
    , 3 :. 3 :. "Docs"    :. Project_status_draft    :. pgArr' []
    , 4 :. 4 :. "Legacy"  :. Project_status_archived :. pgArr' ["backend"] ]
  _ <- insertSch_ (MyAnn "tasks") conn $ rowsOf @TaskIn
    [ 1 :. 1 :. "Design"    :. 10  :. Just (Payload 3) :. pgArr' [2, 3]
    , 1 :. 2 :. "Frontend"  :. 30  :. Just (Payload 8) :. pgArr' [3]
    , 1 :. 3 :. "Polish"    :. 100 :. Nothing          :. pgArr' []
    , 2 :. 1 :. "Schema"    :. 20  :. Just (Payload 5) :. pgArr' [1]
    , 2 :. 2 :. "Endpoints" :. 40  :. Nothing          :. pgArr' [1, 4]
    , 3 :. 1 :. "Outline"   :. 50  :. Nothing          :. pgArr' [] ]
  -- --8<-- [end:sel-seed]
  pure ()

-- --8<-- [start:sel-tree-types]
data Owner = Owner { name :: Text } deriving (Show, Generic)

data Task = Task { seq :: Int32, title :: Text, priority :: Int32 }
  deriving (Show, Generic)

data Project = Project
  { title              :: Text
  , status             :: ProjectStatus
  , projectOwner       :: Owner
  , tasksProjectIdFkey :: [Task]
  } deriving (Show, Generic)
-- --8<-- [end:sel-tree-types]

-- --8<-- [start:sel-self-types]
data Person = Person
  { name         :: Text
  , manager      :: Maybe Owner
  , subordinates :: [Owner]
  } deriving (Show, Generic)
-- --8<-- [end:sel-self-types]

-- --8<-- [start:sel-run]
-- | Выполнить запрос, напечатать строки по одной и параметры запроса,
-- а затем отформатированный SQL.
run :: Show r => String -> IO ([r], (Text, [SomeToField])) -> IO ()
run name act = do
  sql <- section name $ do
    (rows, (sql, params)) <- act
    mapM_ print rows
    putStrLn $ "params: " <> show params
    pure sql
  sqlSection name [sql]
-- --8<-- [end:sel-run]

select :: Connection -> IO ()
select conn = do
  seed conn

  run "sel-tree" do
    -- --8<-- [start:sel-tree]
    selectSch (MyAnn "projects") @Project conn qpEmpty
    -- --8<-- [end:sel-tree]

  run "sel-self" do
    -- --8<-- [start:sel-self]
    selectSch (MyAnn "users") @Person conn qpEmpty
    -- --8<-- [end:sel-self]

  run "sel-cond" do
    -- --8<-- [start:sel-cond]
    selectSch (MyAnn "projects") @("title" := Text :. "status" := ProjectStatus) conn $ qRoot do
      qWhere $ "status" =? Project_status_active &&& "title" ~~? ("%site%" :: Text)
        ||| pnot ("status" =? Project_status_active) &&& pinArr "id" [3, 4 :: Int64]
    -- --8<-- [end:sel-cond]

  run "sel-null" do
    -- --8<-- [start:sel-null]
    selectSch (MyAnn "users") @("name" := Text :. "email" := Maybe Text) conn $ qRoot do
      qWhere $ pnull "email" ||| "email" =?? Just ("bob@example.com" :: Text)
        ||| pin "name" ("Dave" :| ["Eve" :: Text])
    -- --8<-- [end:sel-null]

  run "sel-path" do
    -- --8<-- [start:sel-path]
    selectSch (MyAnn "projects") @Project conn $ qRoot do
      qWhere $ "status" =? Project_status_active
      qOrderBy [descf "title"]
      qPath "tasksProjectIdFkey" do
        qWhere $ "priority" <? (50 :: Int32)
        qOrderBy [descf "priority"]
        qLimit 1
    -- --8<-- [end:sel-path]

  run "sel-pchild" do
    -- --8<-- [start:sel-pchild]
    selectSch (MyAnn "projects") @("title" := Text) conn $ qRoot do
      qWhere $ pchild (Tut "tasks_project_id_fkey") defTabParam ("priority" <? (20 :: Int32))
    -- --8<-- [end:sel-pchild]

  run "sel-pchild-first" do
    -- --8<-- [start:sel-pchild-first]
    selectSch (MyAnn "projects") @("title" := Text) conn $ qRoot do
      qWhere $ pchild (Tut "tasks_project_id_fkey")
        defTabParam { order = [descf "priority"], lo = lo1 }
        ("priority" <? (50 :: Int32))
    -- --8<-- [end:sel-pchild-first]

  run "sel-pparent" do
    -- --8<-- [start:sel-pparent]
    selectSch (MyAnn "tasks") @("title" := Text :. "priority" := Int32) conn $ qRoot do
      qWhere $ pparent (Tut "tasks_project_id_fkey") ("status" =? Project_status_active)
        &&& "priority" >=? (30 :: Int32)
      qOrderBy [ascf "priority"]
    -- --8<-- [end:sel-pparent]

  run "sel-limit" do
    -- --8<-- [start:sel-limit]
    selectSch (MyAnn "tasks") @("title" := Text :. "priority" := Int32) conn $ qRoot do
      qOrderBy [descf "priority"]
      qLimit 2
      qOffset 1
    -- --8<-- [end:sel-limit]

  run "sel-distinct" do
    -- --8<-- [start:sel-distinct]
    selectSch (MyAnn "projects") @("status" := ProjectStatus) conn $ qRoot do
      qDistinct
      qOrderBy [ascf "status"]
    -- --8<-- [end:sel-distinct]

  run "sel-distinct-on" do
    -- --8<-- [start:sel-distinct-on]
    selectSch (MyAnn "tasks") @("projectId" := Int64 :. "title" := Text :. "priority" := Int32) conn $ qRoot do
      qDistinctOn [ascf "projectId"]
      qOrderBy [ascf "priority"]
    -- --8<-- [end:sel-distinct-on]

  run "sel-self-path" do
    -- --8<-- [start:sel-self-path]
    selectSch (MyAnn "users") @Person conn $ qRoot do
      qWhere $ pnull "managerId" ||| "managerId" =? Just (1 :: Int64)
      qOrderBy [ascf "name"]
      qPathToHere "subordinates" do
        qWhere $ "name" ~~? ("%o%" :: Text)
    -- --8<-- [end:sel-self-path]

  run "sel-aggr" do
    -- --8<-- [start:sel-aggr]
    selectSch (MyAnn "tasks")
      @("projectId" := Int64 :. "cnt" := Aggr' ACount Int64 :. "priority" := Aggr' AMax Int32)
      conn $ qRoot do
        qOrderBy [ascf "projectId"]
    -- --8<-- [end:sel-aggr]

  run "sel-aggr-total" do
    -- --8<-- [start:sel-aggr-total]
    selectSch (MyAnn "tasks")
      @("cnt" := Aggr ACount Int64 :. "priority" := Aggr AMax (Maybe Int32))
      conn qpEmpty
    -- --8<-- [end:sel-aggr-total]

  run "sel-json-arr" do
    -- --8<-- [start:sel-json-arr]
    selectSch (MyAnn "tasks")
      @("title" := Text :. "payload" := Maybe Payload :. "watchers" := PgArr Int64)
      conn $ qRoot do
        qWhere $ pnot (pnull "payload")
    -- --8<-- [end:sel-json-arr]

  run "sel-unsafe" do
    -- --8<-- [start:sel-unsafe]
    selectSch (MyAnn "tasks")
      @("title" := Text :. "titleLength" := UnsafeCol '["title"] "length(?)" Int32)
      conn $ qRoot do
        qWhere $ pUnsafeCond do
          watchers <- qual @"watchers"
          pure $ "cardinality(" <> watchers <> ") > 1"
    -- --8<-- [end:sel-unsafe]

-- --8<-- [start:sel-tut]
type Tut name = "tut" ->> name
-- --8<-- [end:sel-tut]
