{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Database.Types.SchList where

import Control.Applicative
import Data.Aeson
import Data.Hashable
import GHC.Exts
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.Schema.Rec
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
    ( Eq, Ord, FromJSON, ToJSON, Hashable, Semigroup, Monoid
    , Functor, Applicative, Alternative, Foldable, IsList
    , Monad )
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

instance CRecordInfo sch t r => CRecordInfo sch t (SchList r) where
  type TRecordInfo sch t (SchList r) = TRecordInfo sch t r
  getRecordInfo = getRecordInfo @sch @t @r
