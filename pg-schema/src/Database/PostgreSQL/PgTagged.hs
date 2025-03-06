{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.PgTagged where

import Data.Aeson
import Data.Coerce
import Data.Hashable
import Data.Kind
import Data.String
import Data.Tagged
import Data.Text as T
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.ToRow as PG
import Database.Schema.Def
import Database.Schema.Rec
import GHC.TypeLits
import PgSchema.Util
import Prelude.Singletons
import Type.Reflection


newtype PgTagged a b = PgTagged (Tagged a b) deriving
  ( Eq, Read, Show, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup )

instance Hashable b => Hashable (PgTagged a b) where
  hashWithSalt s = hashWithSalt @b s . coerce

pattern PgTag :: b -> PgTagged a b
pattern PgTag b = PgTagged (Tagged b)

pgTag :: forall a b. b -> PgTagged a b
pgTag = coerce

unPgTag :: forall a b. PgTagged a b -> b
unPgTag = coerce

rePgTag :: forall a b c. PgTagged a c -> PgTagged b c
rePgTag = coerce

instance (ToStar a, FromJSON b) => FromJSON (PgTagged (a::Symbol) b) where
  parseJSON = withObject "PgTagged " \v ->
    pgTag <$> v .: fromString (T.unpack $ demote @a)

instance (ToStar a, ToJSON b) => ToJSON (PgTagged (a::Symbol) b) where
  toJSON v = object [fromString (T.unpack $ demote @a) .= unPgTag v]

instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) a) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) a) where
  toField = toJSONField

instance ToStar n => CRecordInfo (PgTagged (n::Symbol) r) where
  type TRecordInfo (PgTagged n r) = '[ 'FieldInfo n n]

instance
  CRecordInfo (PgTagged n r) => CRecordInfo (PgTagged ('[n]::[Symbol]) r) where
  type TRecordInfo (PgTagged '[n] r) = TRecordInfo (PgTagged n r)

instance
  (ToStar n, CRecordInfo (PgTagged n r), CRecordInfo (PgTagged (n1 ':ns) r1))
  => CRecordInfo (PgTagged (n ': n1 ':ns ::[Symbol]) (r,r1)) where
  type TRecordInfo (PgTagged (n ': n1 ':ns) (r,r1)) =
    TRecordInfo (PgTagged n r) ++ TRecordInfo (PgTagged (n1 ': ns) r1)

instance CFieldType (PgTagged (n::Symbol) r) n where
  type TFieldType (PgTagged n r) n = r

instance CFieldType (PgTagged ('[n]::[Symbol]) r) n where
  type TFieldType (PgTagged '[n] r) n = r

instance
  CFieldTypeB (n==x) (PgTagged (n ':n1 ':ns) (r,r1)) x
  => CFieldType (PgTagged (n ': n1 ': ns ::[Symbol]) (r,r1)) x where
  type TFieldType (PgTagged (n ':n1 ':ns) (r,r1)) x=
    TFieldTypeB (n==x) (PgTagged (n ':n1 ':ns) (r,r1)) x

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
  ( CSchema sch
  , CQueryFields db sch t
    (FiWithType (TFieldTypeSym1 (PgTagged ns r)) (TRecordInfo (PgTagged ns r))) )
  => CQueryRecord db sch t (PgTagged ns r) where

instance
  ( CSchema sch
  , CInsertFields db sch t
    (FiWithType (TFieldTypeSym1 (PgTagged ns r)) (TRecordInfo (PgTagged ns r)))
  , AllMandatory sch t (PgTagged ns r) '[] )
  => CInsertRecord db sch t (PgTagged ns r) where

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
