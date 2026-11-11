-- --8<-- [start:types-header]
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- --8<-- [end:types-header]
-- | Appendix "Data types": own types for columns.
-- Runs on the chapter 04 data (after its examples).
module Tut.Types (types) where
-- --8<-- [start:types-imports]

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import PgSchema.DML
import Sch (Sch)
-- --8<-- [end:types-imports]
import Tut.Common
import Tut.Output

-- --8<-- [start:types-email]
newtype Email = Email Text
  deriving newtype (Show, Eq, FromField, ToField, FromJSON, ToJSON)

-- Email may be used only for the email column of tut.users
type instance CanConvert1 sch ("tut" ->> "users") "email" ("pg_catalog" ->> "text") ('TypDef "S" x y) Email = ()
-- --8<-- [end:types-email]

-- --8<-- [start:types-inet]
newtype IpAddress = IpAddress Text
  deriving newtype (Show, Eq, FromJSON, ToJSON)

-- FromField for Text accepts only text types, so we read
-- the text form of the inet value ourselves
instance FromField IpAddress where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bs -> pure $ IpAddress (decodeUtf8 bs)

instance ToField IpAddress where
  toField (IpAddress t) = toField t

-- IpAddress fits any column of type inet
type instance CanConvert1 sch tab fld ("pg_catalog" ->> "inet") ('TypDef "I" x y) IpAddress = ()

type AuditAnn t = 'Ann MyRenamer Sch 3 ("audit" ->> t)
-- --8<-- [end:types-inet]

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

  sqlsIp <- section "types-inet" $ do
    -- --8<-- [start:types-inet-use]
    (_, sqlIns) <- insertSch_ (AuditAnn "project_log") conn $ named ["projectId", "message", "clientIp"]
      [ (1 :: Int64) :. ("opened" :: Text) :. Just (IpAddress "192.168.0.10") ]
    (rows, (sql, _)) <- selectSch (AuditAnn "project_log") conn $ qRoot do
      qWhere $ "clientIp" =? Just (IpAddress "192.168.0.10")
    print (plains @("message" := Text :. "clientIp" := Maybe IpAddress) rows)
    -- --8<-- [end:types-inet-use]
    pure [sqlIns, sql]
  sqlSection "types-inet" sqlsIp
