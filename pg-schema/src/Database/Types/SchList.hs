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

instance CRecordInfo r => CRecordInfo (SchList r) where
  type TRecordInfo (SchList r) = TRecordInfo r

instance CFieldType r n => CFieldType (SchList r) n where
  type TFieldType (SchList r) n = TFieldType r n

instance
  ( CSchema sch, CQueryFields db sch t (TRecordInfo (SchList r)) (TFieldTypeSym1 (SchList r))
  , ToStar (TQueryRecord db sch t (SchList r)) )
  => CQueryRecord db sch t (SchList r)

instance
  ( CDmlFields db sch (RdFrom rd) (FiTypeInfo (SchList r)) )
  => CDmlRecordChild db sch rd (SchList r)
