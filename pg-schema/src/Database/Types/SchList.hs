{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Types.SchList where

import Control.Applicative
import Data.Aeson
import Data.Hashable
import GHC.Exts
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.Schema.Def
import Database.Schema.Rec
import Type.Reflection
import PgSchema.Util


newtype SchList a = SchList { getSchList :: [a] } deriving
  ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Semigroup, Monoid
  , Functor, Applicative, Alternative, Foldable, Traversable, IsList
  , Monad )

instance (FromJSON a, Typeable a) => FromField (SchList a) where
  fromField = fromJSONField

instance (ToJSON a) => ToField (SchList a) where
  toField :: ToJSON a => SchList a -> Action
  toField = toJSONField

instance CRecordInfo sch t r => CRecordInfo sch t (SchList r) where
  type TRecordInfo sch t (SchList r) = TRecordInfo sch t r

instance
  ( CSchema sch
  , CQueryFields sch t (TRecordInfo sch t (SchList r))
  , ToStar (TQueryRecord sch t (SchList r)) )
  => CQueryRecord sch t (SchList r)

instance
  ( CSchema sch
  , CDmlFields sch t (TRecordInfo sch t (SchList r))
  , ToStar (TDmlRecord sch t (SchList r)) )
  => CDmlRecord sch t (SchList r)
