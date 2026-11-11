{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Chapter "Tree DML": insertJSON / upsertJSON / updateJSON and transactions.
module Tut.TreeDml (treeDml) where

import Control.Exception (try)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, SqlError (..), withTransaction)
import PgSchema.DML
import Sch
import Tut.Common
import Tut.Output

-- --8<-- [start:tree-types]
type EventIn = "eventNo" := Int32 :. "kind" := Text

type TaskIn = "seq" := Int32 :. "title" := Text :. "priority" := Int32
  :. "taskEventsProjectIdSeqFkey" := [EventIn]

type ProjectIn = "ownerId" := Int64 :. "title" := Text :. "status" := ProjectStatus
  :. "tags" := PgArr Text :. "tasksProjectIdFkey" := [TaskIn]
-- --8<-- [end:tree-types]

-- --8<-- [start:tree-keys]
type ProjectKeys = "id" := Int64
  :. "tasksProjectIdFkey" := [ "seq" := Int32 :. "taskEventsProjectIdSeqFkey" := ["eventNo" := Int32] ]
-- --8<-- [end:tree-keys]

-- --8<-- [start:tree-patch-types]
type TaskPatch = "seq" := Int32 :. "title" := Text
  :. "taskEventsProjectIdSeqFkey" := [EventIn]

type ProjectPatch = "id" := Int64 :. "tasksProjectIdFkey" := [TaskPatch]

type PatchOut = Maybe ("id" := Int64 :. "tasksProjectIdFkey" := ["seq" := Int32 :. "title" := Text])
-- --8<-- [end:tree-patch-types]

-- --8<-- [start:tree-update-types]
type TaskUpd = "seq" := Int32 :. "priority" := Int32

type ProjectUpd = "id" := Int64 :. "status" := ProjectStatus
  :. "tasksProjectIdFkey" := [TaskUpd]

type UpdOut = Maybe ("id" := Int64 :. "status" := ProjectStatus
  :. "tasksProjectIdFkey" := [Maybe ("seq" := Int32 :. "priority" := Int32)])
-- --8<-- [end:tree-update-types]

-- --8<-- [start:tree-count]
-- | How many projects have this title.
countProjects :: Connection -> Text -> IO Int64
countProjects conn title = do
  (rows, _) <- selectSch (MyAnn "projects") conn $ qRoot $ qWhere $ "title" =? title
  pure $ sum $ unAggr <$> plains @("cnt" := Aggr ACount Int64) rows
-- --8<-- [end:tree-count]

treeDml :: Connection -> IO ()
treeDml conn = do
  _ <- insertSch_ (MyAnn "users") conn $ named ["name"] ["Alice" :: Text]

  sqls1 <- section "tree-insert" $ do
    -- --8<-- [start:tree-insert]
    (keys, sqlIns) <- insertJSON (MyAnn "projects") conn $ rowsOf @ProjectIn
      [ 1 :. "Website" :. Project_status_active :. pgArr' ["web"] :.
          [ 1 :. "Design" :. 10 :. [ 1 :. "created", 2 :. "assigned" ]
          , 2 :. "Frontend" :. 30 :. [] ]
      , 1 :. "API" :. Project_status_draft :. pgArr' [] :.
          [ 1 :. "Schema" :. 20 :. [ 1 :. "created" ] ] ]
    mapM_ print (keys :: [ProjectKeys])
    -- --8<-- [end:tree-insert]
    pure [sqlIns]
  sqlSection "tree-insert" sqls1

  -- --8<-- [start:tree-insert-nr]
  sqlInsNr <- insertJSON_ (MyAnn "projects") conn $ rowsOf @ProjectIn
    [ 1 :. "Docs" :. Project_status_draft :. pgArr' [] :. [] ]
  -- --8<-- [end:tree-insert-nr]
  sqlSection "tree-insert-nr" [sqlInsNr]

  sqls3 <- section "tree-upsert" $ do
    -- --8<-- [start:tree-upsert]
    (res, sqlUps) <- upsertJSON (MyAnn "projects") conn $ rowsOf @ProjectPatch
      [ 1 :.
          [ 1 :. "Design v2" :. [ 3 :. "renamed" ]
          , 3 :. "Backend" :. [] ] ]
    mapM_ print (res :: [PatchOut])
    -- --8<-- [end:tree-upsert]
    pure [sqlUps]
  sqlSection "tree-upsert" sqls3

  section "tree-upsert-missing" $ do
    -- --8<-- [start:tree-upsert-missing]
    (res, _) <- upsertJSON (MyAnn "projects") conn $ rowsOf @ProjectPatch
      [ 999 :. [ 1 :. "Ghost" :. [] ] ]
    print (res :: [PatchOut])
    -- --8<-- [end:tree-upsert-missing]

  sqls4 <- section "tree-update" $ do
    -- --8<-- [start:tree-update]
    (res, sqlUpd) <- updateJSON (MyAnn "projects") conn $ rowsOf @ProjectUpd
      [ 2 :. Project_status_active :. [ 1 :. 5, 9 :. 50 ]
      , 999 :. Project_status_active :. [] ]
    mapM_ print (res :: [UpdOut])
    -- --8<-- [end:tree-update]
    pure [sqlUpd]
  sqlSection "tree-update" sqls4

  section "tree-tx" $ do
    -- --8<-- [start:tree-tx]
    let
      docs = rowsOf @("id" := Int64 :. ProjectIn)
        [ 100 :. 1 :. "Tx demo" :. Project_status_draft :. pgArr' [] :. [] ]
    res <- try @SqlError $ withTransaction conn do
      _ <- insertJSON_ (MyAnn "projects") conn docs
      insertJSON_ (MyAnn "projects") conn docs  -- the same id: an error
    putStrLn $ either (("error: " <>) . show . sqlErrorMsg) (const "ok") res
    cnt <- countProjects conn "Tx demo"
    putStrLn $ "projects \"Tx demo\": " <> show cnt
    -- --8<-- [end:tree-tx]
