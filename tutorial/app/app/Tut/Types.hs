{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Приложение «Типы данных»: свой скалярный тип для колонки.
-- Выполняется на данных главы 04 (после её примеров).
module Tut.Types (types) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import PgSchema.DML
import Tut.Common
import Tut.Output

-- --8<-- [start:types-email]
newtype Email = Email Text
  deriving newtype (Show, Eq, FromField, ToField, FromJSON, ToJSON)

-- Email можно использовать только для колонки email таблицы tut.users
type instance CanConvert1 sch ("tut" ->> "users") "email" ("pg_catalog" ->> "text") ('TypDef "S" x y) Email = ()
-- --8<-- [end:types-email]

types :: Connection -> IO ()
types conn = do
  sqls <- section "types-email" $ do
    -- --8<-- [start:types-email-use]
    _ <- insertSch_ (MyAnn "users") conn $ named ["id", "name", "email"]
      [ (5 :: Int64) :. ("Eve" :: Text) :. Just (Email "eve@example.com") ]
    (rows, (sql, _)) <- selectSch (MyAnn "users") conn $ qRoot do
      qWhere $ "email" =? Just (Email "eve@example.com")
    print (plains @("name" := Text :. "email" := Maybe Email) rows)
    -- --8<-- [end:types-email-use]
    pure [sql]
  sqlSection "types-email" sqls
