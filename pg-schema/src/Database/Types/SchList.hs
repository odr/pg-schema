{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Database.Types.SchList where

import Control.Applicative
import Data.Aeson
#ifdef MK_HASHABLE
import Data.Hashable
#endif
import GHC.Exts
import GHC.TypeLits
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.PgTagged
-- import Database.Schema.Rec
import PgSchema.Util
import Type.Reflection
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat
#endif


newtype SchList a = SchList { getSchList :: [a] }
  deriving stock (Traversable, Show)
  deriving newtype
    ( Eq, Ord, FromJSON, ToJSON, Semigroup, Monoid
    , Functor, Applicative, Alternative, Foldable, Monad, IsList
#ifdef MK_HASHABLE
    , Hashable )
#else
    )
#endif
#ifdef MK_ARBITRARY
  deriving newtype Arbitrary
#endif
#ifdef MK_FLAT
  deriving newtype Flat
#endif

instance (FromJSON a, Typeable a) => FromField (SchList a) where
  fromField = fromJSONField

instance (ToJSON a) => ToField (SchList a) where
  toField :: ToJSON a => SchList a -> Action
  toField = toJSONField

instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) (SchList a)) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) (SchList a)) where
  toField = toJSONField
