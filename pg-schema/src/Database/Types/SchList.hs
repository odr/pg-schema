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

instance CFieldType sch r n => CFieldType sch (SchList r) n where
  type TFieldType sch (SchList r) n = TFieldType sch r n

instance
  ( CSchema sch
  , CQueryFields db sch t (TRecordInfo sch t (SchList r))
    (TFieldTypeSym2 sch (SchList r))
  , ToStar (TQueryRecord db sch t (SchList r)) )
  => CQueryRecord db sch t (SchList r)

instance
  ( CDmlFields db sch (RdFrom rd) (TRecordInfo sch (RdFrom rd) (SchList r))
    (TFieldTypeSym2 sch (SchList r)) )
  => CDmlRecordChild db sch rd (SchList r)
