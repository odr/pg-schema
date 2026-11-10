{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Глава «DML операции», операции со связанными таблицами:
-- insertJSON / upsertJSON / updateJSON.
module Tut.TreeDml (treeDml, treeCompare) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
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

-- | Изменение: ключ проекта, ключи задач и новые значения.
type TaskPatch = "seq" := Int32 :. "title" := Text
  :. "taskEventsProjectIdSeqFkey" := [EventIn]

type ProjectPatch = "id" := Int64 :. "tasksProjectIdFkey" := [TaskPatch]
-- --8<-- [end:tree-types]

treeDml :: Connection -> IO ()
treeDml conn = do
  -- --8<-- [start:tree-in]
  let
    treeIn = rowsOf @ProjectIn
      [ 1 :. "tree-demo" :. Project_status_active :. pgArr' ["nested"]
        :. [ 1 :. "task-1" :. 10 :. [ 1 :. "created" ] ] ]
  -- --8<-- [end:tree-in]

  section "insert-json-nr" $ do
    -- --8<-- [start:insert-json-nr]
    tInsJ0 <- insertJSON_ (MyAnn "projects") conn treeIn
    putStrLn $ "insertJSON_ text: " <> T.unpack tInsJ0
    -- --8<-- [end:insert-json-nr]

  section "insert-json" $ do
    -- --8<-- [start:insert-json]
    (resJ1, tInsJ1) <- insertJSON (MyAnn "projects") conn treeIn
    putStrLn $ "insertJSON text: " <> T.unpack tInsJ1
    putStrLn $ "insertJSON result: "
      <> show (plains @("id" := Int64 :. "tasksProjectIdFkey" := ["projectId" := Int64 :. "seq" := Int32]) resJ1)
    -- --8<-- [end:insert-json]

  -- --8<-- [start:tree-ups]
  let
    treeUps = rowsOf @ProjectPatch
      [ 1 :. [ 1 :. "task-1-updated" :. [ 1 :. "updated" ] ] ]
  -- --8<-- [end:tree-ups]

  section "upsert-json-nr" $ do
    -- --8<-- [start:upsert-json-nr]
    tUpsJ0 <- upsertJSON_ (MyAnn "projects") conn treeUps
    putStrLn $ "upsertJSON_ text: " <> T.unpack tUpsJ0
    -- --8<-- [end:upsert-json-nr]

  section "upsert-json" $ do
    -- --8<-- [start:upsert-json]
    (resUpsJ1, tUpsJ1) <- upsertJSON (MyAnn "projects") conn treeUps
    putStrLn $ "upsertJSON text: " <> T.unpack tUpsJ1
    putStrLn $ "upsertJSON result: "
      <> show (plains @("id" := Int64 :. "tasksProjectIdFkey" := ["seq" := Int32 :. "title" := Text]) resUpsJ1)
    -- --8<-- [end:upsert-json]

-- | Один и тот же patch по проекту: insert / upsert / update.
treeCompare :: Connection -> IO ()
treeCompare conn = section "tree-compare" $ do
  -- --8<-- [start:tree-compare]
  let
    -- явный id, чтобы повторная вставка дала конфликт по первичному ключу
    treeFull = rowsOf @("id" := Int64 :. ProjectIn)
      [ 100 :. 1 :. "tree-full" :. Project_status_active :. pgArr' ["demo"]
        :. [ 1 :. "task-a" :. 10 :. [ 1 :. "created" ] ] ]
    patch projectId = rowsOf @ProjectPatch
      [ projectId :. [ 1 :. "task-patched" :. [ 1 :. "patched" ] ] ]
    treePatch = patch 100
    treePatchBad = patch 99999
  tIns <- insertJSON_ (MyAnn "projects") conn treeFull
  putStrLn $ "insertJSON ok: " <> T.unpack (T.take 60 tIns)
  putStrLn "insertJSON again (expect PG duplicate error):"
  void $
    insertJSON_ (MyAnn "projects") conn treeFull `catch`
      \(e :: SomeException) -> do
        putStrLn $ "  caught: " <> show e
        pure ""
  tUps <- upsertJSON_ (MyAnn "projects") conn treePatch
  putStrLn $ "upsertJSON patch ok: " <> T.unpack (T.take 60 tUps)
  tUpd <- updateJSON_ (MyAnn "projects") conn treePatch
  putStrLn $ "updateJSON patch ok: " <> T.unpack (T.take 60 tUpd)
  tUpsBad <- upsertJSON_ (MyAnn "projects") conn treePatchBad
  putStrLn $ "upsertJSON bad id (no new row): " <> T.unpack (T.take 60 tUpsBad)
  tUpdBad <- updateJSON_ (MyAnn "projects") conn treePatchBad
  putStrLn $ "updateJSON bad id: " <> T.unpack (T.take 60 tUpdBad)
  -- --8<-- [end:tree-compare]
