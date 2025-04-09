module Database.Types.EmptyField where

import Data.Aeson
import Data.Hashable
import Data.Void
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG


newtype EmptyField = EmptyField (Maybe Void) -- Maybe to omitNothingField in JSON
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON)

instance FromField EmptyField where
  fromField _ _ = pure $ EmptyField Nothing

instance ToField EmptyField where
  toField _ = toField Null
