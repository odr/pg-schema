{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Types.SchList where

import Control.Applicative
import Data.Aeson
import Data.Hashable
import GHC.Exts
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG
-- import Database.Schema.Rec
import Type.Reflection


newtype SchList a = SchList { getSchList :: [a] } deriving
  ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Semigroup, Monoid
  , Functor, Applicative, Alternative, Foldable, Traversable, IsList )

instance (FromJSON a, Typeable a) => FromField (SchList a) where
  fromField = fromJSONField

instance (ToJSON a) => ToField (SchList a) where
  toField :: ToJSON a => SchList a -> Action
  toField = toJSONField
