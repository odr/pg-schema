{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Chapter "Single-table DML": insert / update / upsert / delete.
module Tut.FlatDml (flatDml) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import PgSchema.DML
import Sch
import Tut.Common
import Tut.Output

flatDml :: Connection -> IO ()
flatDml conn = do
  sql0 <- section "insert-projects" $ do
    -- --8<-- [start:insert-projects]
    (res, tIns) <- insertSch (MyAnn "projects") conn $
      named ["ownerId", "title", "status", "tags"]
        [ (1 :: Int64) :. ("pg-schema" :: Text) :. Project_status_active :. pgArr' ["db" :: Text, "haskell"]
        , 1 :. "tutorial" :. Project_status_draft :. pgArr' ["learning"] ]
    putStrLn $ "insert text: " <> T.unpack tIns
    putStrLn $ "inserted ids: " <> show (plains @("id" := Int64) res)
    -- --8<-- [end:insert-projects]
    pure [tIns]
  sqlSection "insert-projects" sql0

  section "update-by-cond" $ do
    -- --8<-- [start:update-by-cond]
    res <- updateByCond (MyAnn "projects") conn
      ("status" =: Project_status_archived)
      ("id" =? (2 :: Int64))
    putStrLn $ "updated: " <> show (plains @("id" := Int64 :. "title" := Text :. "status" := ProjectStatus) res)
    -- --8<-- [end:update-by-cond]

  sql1 <- section "update-text" $ do
    -- --8<-- [start:update-text]
    let
      (tUpd :: Text, updParams) = updateText (MyAnn "projects")
        @("status" := ProjectStatus)
        @("id" := Int64 :. "title" := Text :. "status" := ProjectStatus)
        ("id" =? (2 :: Int64))
    putStrLn $ "update text: " <> T.unpack tUpd
    putStrLn $ "update params: " <> show updParams
    -- --8<-- [end:update-text]
    pure [tUpd]
  sqlSection "update-text" sql1

  sql2 <- section "delete-by-cond" $ do
    -- --8<-- [start:delete-by-cond]
    (cnt5, tDel5) <- deleteByCond (MyAnn "projects") conn $ "id" =? (2 :: Int64)
    putStrLn $ "delete text: " <> T.unpack (fst tDel5)
    putStrLn $ "delete params: " <> show (snd tDel5)
    putStrLn $ "deleted: " <> show cnt5 <> " rows"
    -- --8<-- [end:delete-by-cond]
    pure [fst tDel5]
  sqlSection "delete-by-cond" sql2

  sql3 <- section "update-by-key" $ do
    -- --8<-- [start:update-by-key]
    (res :: [Maybe ("id" := Int64 :. "name" := Text)], tUpd) <-
      updateByKey (MyAnn "users") conn $ named ["id", "name"]
        [ (2 :: Int64) :. ("Quentin T." :: Text)
        , 999 :. "Nobody" ]
    putStrLn $ "update text: " <> T.unpack tUpd
    putStrLn $ "updated: " <> show res
    -- --8<-- [end:update-by-key]
    pure [tUpd]
  sqlSection "update-by-key" sql3

  sql4 <- section "upsert-by-pk" $ do
    -- --8<-- [start:upsert-by-pk]
    (res, tUps) <- upsertByKey (MyAnn "users") conn $ named ["id", "name"]
      [ (3 :: Int64) :. ("Jason B." :: Text)
      , 100 :. "Margo" ]
    putStrLn $ "upsert text: " <> T.unpack tUps
    putStrLn $ "upserted: " <> show (plains @("id" := Int64 :. "name" := Text) res)
    -- --8<-- [end:upsert-by-pk]
    pure [tUps]
  sqlSection "upsert-by-pk" sql4

  sql5 <- section "upsert-by-uk" $ do
    -- --8<-- [start:upsert-by-uk]
    (res, tUps) <- upsertByKey (MyAnn "users") conn $ named ["name", "email"]
      [ ("Quentin Tarantino" :: Text) :. Just ("quentin@example.com" :: Text)
      , "Mia" :. Just "mia@example.com" ]
    putStrLn $ "upsert text: " <> T.unpack tUps
    putStrLn $ "upserted: " <> show (plains @("id" := Int64 :. "name" := Text :. "email" := Maybe Text) res)
    -- --8<-- [end:upsert-by-uk]
    pure [tUps]
  sqlSection "upsert-by-uk" sql5
