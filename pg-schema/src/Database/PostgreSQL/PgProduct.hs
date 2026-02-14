{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Database.PostgreSQL.PgProduct where

import Data.Aeson
import Data.Data (Typeable)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics


data a :.. b = a :.. b deriving (Generic, Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Generically (a :.. b))
-- ^ ':.' but with all needed instances

infixr 1 :..

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
