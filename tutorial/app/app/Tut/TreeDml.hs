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

treeDml :: Connection -> IO ()
treeDml conn = do
  -- --8<-- [start:tree-in]
  let
    treeIn =
      [ "ownerId" =: (1 :: Int64)
        :. "title" =: ("tree-demo" :: Text)
        :. "status" =: Project_status_active
        :. "tags" =: pgArr' ["nested" :: Text]
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-1" :: Text)
            :. "priority" =: (10 :: Int32)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("created" :: Text)
              ]
          ]
      ]
  -- --8<-- [end:tree-in]

  section "insert-json-nr" $ do
    -- --8<-- [start:insert-json-nr]
    tInsJ0 <- insertJSON_ (MyAnn "projects") conn treeIn
    putStrLn $ "insertJSON_ text: " <> T.unpack tInsJ0
    -- --8<-- [end:insert-json-nr]

  section "insert-json" $ do
    -- --8<-- [start:insert-json]
    (resJ1 :: ["id" := Int64 :. "tasksProjectIdFkey" := ["projectId" := Int64 :. "seq" := Int32]], tInsJ1) <-
      insertJSON (MyAnn "projects") conn treeIn
    putStrLn $ "insertJSON text: " <> T.unpack tInsJ1
    putStrLn $ "insertJSON result: " <> show resJ1
    -- --8<-- [end:insert-json]

  -- --8<-- [start:tree-ups]
  let
    treeUps =
      [ "id" =: (1 :: Int64)
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-1-updated" :: Text)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("updated" :: Text)
              ]
          ]
      ]
  -- --8<-- [end:tree-ups]

  section "upsert-json-nr" $ do
    -- --8<-- [start:upsert-json-nr]
    tUpsJ0 <- upsertJSON_ (MyAnn "projects") conn treeUps
    putStrLn $ "upsertJSON_ text: " <> T.unpack tUpsJ0
    -- --8<-- [end:upsert-json-nr]

  section "upsert-json" $ do
    -- --8<-- [start:upsert-json]
    (resUpsJ1 :: ["id" := Int64 :. "tasksProjectIdFkey" := ["seq" := Int32 :. "title" := Text]], tUpsJ1) <-
      upsertJSON (MyAnn "projects") conn treeUps
    putStrLn $ "upsertJSON text: " <> T.unpack tUpsJ1
    putStrLn $ "upsertJSON result: " <> show resUpsJ1
    -- --8<-- [end:upsert-json]

-- | Один и тот же patch по проекту: insert / upsert / update.
treeCompare :: Connection -> IO ()
treeCompare conn = section "tree-compare" $ do
  -- --8<-- [start:tree-compare]
  let
    -- явный id, чтобы повторная вставка дала конфликт по первичному ключу
    treeFull =
      [ "id" =: (100 :: Int64)
        :. "ownerId" =: (1 :: Int64)
        :. "title" =: ("tree-full" :: Text)
        :. "status" =: Project_status_active
        :. "tags" =: pgArr' ["demo" :: Text]
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-a" :: Text)
            :. "priority" =: (10 :: Int32)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("created" :: Text)
              ]
          ]
      ]
    patch (projectId :: Int64) =
      [ "id" =: projectId
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-patched" :: Text)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("patched" :: Text)
              ]
          ]
      ]
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
