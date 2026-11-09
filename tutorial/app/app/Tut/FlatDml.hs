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

  section "update-text" $ do
    -- --8<-- [start:update-text]
    let
      (tUpd5 :: Text, updParams5) = updateText_ (MyAnn "projects")
        @("status" := PGEnum Sch ("tut" ->> "project_status")) ("id" =? (2 :: Int64))
    putStrLn $ "update text: " <> T.unpack tUpd5
    putStrLn $ "update params: " <> show updParams5
    -- --8<-- [end:update-text]

  section "delete-by-cond" $ do
    -- --8<-- [start:delete-by-cond]
    (cnt5, tDel5) <- deleteByCond (MyAnn "projects") conn $ "id" =? (2 :: Int64)
    putStrLn $ "delete text: " <> T.unpack (fst tDel5)
    putStrLn $ "delete params: " <> show (snd tDel5)
    putStrLn $ "deleted: " <> show cnt5 <> " rows"
    -- --8<-- [end:delete-by-cond]
