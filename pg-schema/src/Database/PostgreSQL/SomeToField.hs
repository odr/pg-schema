module Database.PostgreSQL.SomeToField where

import Database.PostgreSQL.Simple.ToField


-- | Unnamed Fields. Mostly used in conditions to pass parameters
data SomeToField where
  SomeToField :: (ToField a, Show a) => a -> SomeToField

deriving instance Show SomeToField

instance ToField SomeToField where
  toField (SomeToField v) = toField v
