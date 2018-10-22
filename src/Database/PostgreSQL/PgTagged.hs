{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.PgTagged where

import Data.Aeson
import Data.Coerce
import Data.Kind
import Data.Singletons.Prelude
import Data.Tagged
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.ToRow as PG
import Database.Schema.Rec
import GHC.TypeLits
import Type.Reflection
import Util.ToStar


#if MIN_VERSION_base(4,11,0)
newtype PgTagged a b = PgTagged (Tagged a b) deriving
  ( Eq, Read, Show, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup)
#else
newtype PgTagged a b = PgTagged (Tagged a b) deriving
  ( Eq, Read, Show, Ord, Functor, Applicative, Monad, Foldable, Monoid)
#endif

pgTag :: b -> PgTagged a b
pgTag = coerce

unPgTag :: PgTagged a b -> b
unPgTag = coerce

rePgTag :: PgTagged a c -> PgTagged b c
rePgTag = coerce

instance (ToStar a, FromJSON b) => FromJSON (PgTagged (a::Symbol) b) where
  parseJSON = withObject "PgTagged " $ \v ->
    pgTag <$> v .: toStar @_ @a

instance (ToStar a, ToJSON b) => ToJSON (PgTagged (a::Symbol) b) where
  toJSON v = object [toStar @_ @a .= unPgTag v]

-- instance (ToStar a, FromJSON b) => FromJSON (PgTagged ([a]::[Symbol]) b) where
--   parseJSON = rePgTag @a <$> parseJSON
--
-- instance (ToStar a, ToJSON b) => ToJSON (PgTagged ([a]::[Symbol]) b) where
--   toJSON v = object [toStar @_ @a .= unPgTag v]


instance ToStar n => CRecordInfo (PgTagged (n::Symbol) r) where
  type TRecordInfo (PgTagged n r) = '[ 'FieldInfo 'FldPlain n n]

instance
  CRecordInfo (PgTagged n r) => CRecordInfo (PgTagged ('[n]::[Symbol]) r) where
  type TRecordInfo (PgTagged '[n] r) = TRecordInfo (PgTagged n r)

instance
  (ToStar n, CRecordInfo (PgTagged n r), CRecordInfo (PgTagged (n1 ':ns) r1))
  => CRecordInfo (PgTagged (n ': n1 ':ns ::[Symbol]) (r,r1)) where
#if MIN_VERSION_singletons(2,4,0)
  type TRecordInfo (PgTagged (n ': n1 ':ns) (r,r1)) =
    TRecordInfo (PgTagged n r) ++ TRecordInfo (PgTagged (n1 ': ns) r1)
#else
  type TRecordInfo (PgTagged (n ': n1 ':ns) (r,r1)) =
    TRecordInfo (PgTagged n r) :++ TRecordInfo (PgTagged (n1 ': ns) r1)
#endif

instance CFieldType (PgTagged (n::Symbol) r) n where
  type TFieldType (PgTagged n r) n = r

instance CFieldType (PgTagged ('[n]::[Symbol]) r) n where
  type TFieldType (PgTagged '[n] r) n = r

#if MIN_VERSION_singletons(2,4,0)
instance
  CFieldTypeB (n==x) (PgTagged (n ':n1 ':ns) (r,r1)) x
  => CFieldType (PgTagged (n ': n1 ': ns ::[Symbol]) (r,r1)) x where
  type TFieldType (PgTagged (n ':n1 ':ns) (r,r1)) x=
    TFieldTypeB (n==x) (PgTagged (n ':n1 ':ns) (r,r1)) x
#else
instance
  CFieldTypeB (n:==x) (PgTagged (n ':n1 ':ns) (r,r1)) x
  => CFieldType (PgTagged (n ': n1 ': ns ::[Symbol]) (r,r1)) x where
  type TFieldType (PgTagged (n ':n1 ':ns) (r,r1)) x=
    TFieldTypeB (n:==x) (PgTagged (n ': n1 ':ns) (r,r1)) x
#endif

class CFieldTypeB (b :: Bool) (r :: Type) (n :: Symbol) where
  type TFieldTypeB b r n :: Type

instance CFieldTypeB 'True (PgTagged (n ':ns) (r,r1)) n where
  type TFieldTypeB 'True (PgTagged (n ':ns) (r,r1)) n = r

instance
  CFieldType (PgTagged ns r1) n1
  => CFieldTypeB 'False (PgTagged (n ':ns) (r,r1)) n1 where
  type TFieldTypeB 'False (PgTagged (n ':ns) (r,r1)) n1 =
    TFieldType (PgTagged ns r1) n1

instance
  ( ToStar t
  , CQueryFields db sch t (PgTagged ns r) (TRecordInfo (PgTagged ns r)) )
  => CQueryRecord db sch (t::Symbol) (PgTagged ns r) where

instance FromRow (Only b) => FromRow (PgTagged (n::Symbol) b) where
  fromRow = coerce @(Only b) <$> fromRow

instance FromRow (Only b) => FromRow (PgTagged ('[n]::[Symbol]) b) where
  fromRow = coerce @(Only b) <$> fromRow

instance (FromRow (Only a), FromRow (PgTagged (n2 ': ns) as))
  => FromRow (PgTagged (n1 ': n2 ': ns) (a,as)) where
    fromRow =
      coerce @(Only a, PgTagged (n2 ': ns) as) <$> ((,) <$> fromRow <*> fromRow)

instance ToRow (Only b) => ToRow (PgTagged (n::Symbol) b) where
  toRow = toRow @(Only b) . coerce

instance ToRow (Only b) => ToRow (PgTagged ('[n]::[Symbol]) b) where
  toRow = toRow @(Only b) . coerce

instance (ToRow (Only a), ToRow (PgTagged (n2 ': ns) as))
  => ToRow (PgTagged (n1 ': n2 ': ns) (a,as)) where
    toRow
      = toRow @(Only a PG.:. PgTagged (n2 ': ns) as)
      . ((PG.:.) <$> fst <*> snd) . coerce

instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) a) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) a) where
  toField = toJSONField
