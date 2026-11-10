{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Глава «DML операции»: плоские insert / update / delete.
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
  section "insert-projects" $ do
    -- --8<-- [start:insert-projects]
    (res4 :: ["id" := Int64], tIns4) <- insertSch (MyAnn "projects") conn
      [ "ownerId" =: (1 :: Int64) :. "title" =: ("pg-schema" :: Text)
        :. "status" =: Project_status_active
        :. "tags" =: pgArr' ["db" :: Text, "haskell"]
      , "ownerId" =: 1 :. "title" =: "tutorial"
        :. "status" =: Project_status_draft :. "tags" =: pgArr' ["learning"] ]
    putStrLn $ "insert text: " <> T.unpack tIns4
    putStrLn $ "inserted: " <> show res4
    -- --8<-- [end:insert-projects]

  section "update-by-cond" $ do
    -- --8<-- [start:update-by-cond]
    (res :: ["id" := Int64 :. "title" := Text :. "status" := PGEnum Sch ("tut" ->> "project_status")]) <-
      updateByCond (MyAnn "projects") conn
        ("status" =: Project_status_archived)
        ("id" =? (2 :: Int64))
    putStrLn $ "updated: " <> show res
    -- --8<-- [end:update-by-cond]

  section "update-text" $ do
    -- --8<-- [start:update-text]
    let
      (tUpd :: Text, updParams) = updateText (MyAnn "projects")
        @("status" := PGEnum Sch ("tut" ->> "project_status"))
        @("id" := Int64 :. "title" := Text :. "status" := PGEnum Sch ("tut" ->> "project_status"))
        ("id" =? (2 :: Int64))
    putStrLn $ "update text: " <> T.unpack tUpd
    putStrLn $ "update params: " <> show updParams
    -- --8<-- [end:update-text]

  section "delete-by-cond" $ do
    -- --8<-- [start:delete-by-cond]
    (cnt5, tDel5) <- deleteByCond (MyAnn "projects") conn $ "id" =? (2 :: Int64)
    putStrLn $ "delete text: " <> T.unpack (fst tDel5)
    putStrLn $ "delete params: " <> show (snd tDel5)
    putStrLn $ "deleted: " <> show cnt5 <> " rows"
    -- --8<-- [end:delete-by-cond]

  section "update-by-key" $ do
    -- --8<-- [start:update-by-key]
    (res :: [Maybe ("id" := Int64 :. "name" := Text)], tUpd) <-
      updateByKey (MyAnn "users") conn
        [ "id" =: (2 :: Int64) :. "name" =: ("Quentin T." :: Text)
        , "id" =: 999 :. "name" =: "Nobody" ]
    putStrLn $ "update text: " <> T.unpack tUpd
    putStrLn $ "updated: " <> show res
    -- --8<-- [end:update-by-key]

  section "upsert-by-pk" $ do
    -- --8<-- [start:upsert-by-pk]
    (res :: ["id" := Int64 :. "name" := Text], tUps) <-
      upsertByKey (MyAnn "users") conn
        [ "id" =: (3 :: Int64) :. "name" =: ("Jason B." :: Text)
        , "id" =: 100 :. "name" =: "Margo" ]
    putStrLn $ "upsert text: " <> T.unpack tUps
    putStrLn $ "upserted: " <> show res
    -- --8<-- [end:upsert-by-pk]

  section "upsert-by-uk" $ do
    -- --8<-- [start:upsert-by-uk]
    (res :: ["id" := Int64 :. "name" := Text :. "email" := Maybe Text], tUps) <-
      upsertByKey (MyAnn "users") conn
        [ "name" =: ("Quentin Tarantino" :: Text) :. "email" =: Just ("quentin@example.com" :: Text)
        , "name" =: "Mia" :. "email" =: Just "mia@example.com" ]
    putStrLn $ "upsert text: " <> T.unpack tUps
    putStrLn $ "upserted: " <> show res
    -- --8<-- [end:upsert-by-uk]
