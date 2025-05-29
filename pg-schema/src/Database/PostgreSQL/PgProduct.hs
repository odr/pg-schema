{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.PgProduct where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.Schema.Rec
import GHC.Generics
import Prelude.Singletons as SP
import Data.Data (Typeable)

data a :.. b = a :.. b deriving (Generic, Show)
-- ^ ':.' but with all needed instances

instance (CRecordInfo sch t r1, CRecordInfo sch t r2) =>
  CRecordInfo sch t (r1 :.. r2) where
  type TRecordInfo sch t (r1 :.. r2) =
    TRecordInfo sch t r1 ++ TRecordInfo sch t r2
  getRecordInfo = RecordInfo
    { tabName = r1.tabName
    , fields = r1.fields <> r2.fields }
    where
      r1 = getRecordInfo @sch @t @r1
      r2 = getRecordInfo @sch @t @r2

instance (FromJSON a, FromJSON b) => FromJSON (a :.. b) where
  parseJSON v = do
    a <- parseJSON v
    b <- parseJSON v
    pure $ a :.. b

instance (ToJSON a, ToJSON b) => ToJSON (a :.. b) where
  toJSON (a :.. b) = case (toJSON a, toJSON b) of
    (Object oa, Object ob) -> Object $ oa <> ob
    _ -> error
      "try to convert to JSON a pair (:..) but one element is not an object"


instance (ToRow a, ToRow b) => ToRow (a :.. b) where
  toRow (a :.. b) = toRow $ a :. b

instance (FromRow a, FromRow b) => FromRow (a :.. b) where
  fromRow = do
    (a :. b) <- fromRow
    pure $ a :.. b

instance (FromJSON (a :.. b), Typeable (a :.. b)) => FromField (a :.. b) where
  fromField = fromJSONField

instance ToJSON (a :.. b) => ToField (a :.. b) where toField = toJSONField
