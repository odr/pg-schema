{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Глава «Рабочая модель pg-schema»: первые insert/select, Generic и PgTag.
module Tut.Basics (basics) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import PgSchema.DML
import Sch ()
import Tut.Common
import Tut.Output

basics :: Connection -> IO ()
basics conn = do
  section "users-generic" $ do
    -- --8<-- [start:users-generic]
    (cnt, tIns) <- insertSch_ (MyAnn "users") conn [MkUser "Benjy" Nothing]
    (res, (tSel, selParams)) <- selectSch (MyAnn "users") @User conn qpEmpty
    putStrLn $ "insert text: " <> T.unpack tIns
    putStrLn $ "inserted " <> show cnt <> " rows"
    putStrLn $ "select text: " <> T.unpack tSel
    putStrLn $ "select params: " <> show selParams
    putStrLn $ "selected rows: " <> show res
    -- --8<-- [end:users-generic]

  section "users-pgtag" $ do
    -- --8<-- [start:users-pgtag]
    (res2 :: ["id" := Int64 :. "managerId" := Maybe Int64 :. User], tIns2) <-
      insertSch (MyAnn "users") conn
        [ MkUser "Quentin" (Just "quentin@example.com")
        , MkUser "Jason" (Just "jason@example.com") ]
    (res3 :: ["name" := Text], (tSel3, selParams3)) <- selectSch (MyAnn "users") conn qpEmpty
    putStrLn $ "insert text: " <> T.unpack tIns2
    putStrLn $ "inserted: " <> show res2
    putStrLn $ "select text: " <> T.unpack tSel3
    putStrLn $ "select params: " <> show selParams3
    putStrLn $ "selected rows: " <> show res3
    -- --8<-- [end:users-pgtag]
