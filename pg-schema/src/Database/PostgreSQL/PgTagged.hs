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


newtype PgTagged a b = PgTagged (Tagged a b)
  deriving
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

instance (ToStar a, FromJSON b) => FromJSON (PgTagged ('[a]::[Symbol]) b) where
  parseJSON = withObject "PgTagged " \v ->
    pgTag <$> v .: fromString (T.unpack $ demote @a)

instance (FromJSON (PgTagged n r), FromJSON (PgTagged (n1 ': ns) r1))
  => FromJSON (PgTagged (n ': n1 ': ns::[Symbol]) (r,r1)) where
  parseJSON v = do
    o1 <- parseJSON @(PgTagged n r) v
    o2 <- parseJSON @(PgTagged (n1 ': ns) r1) v
    pure $ pgTag (unPgTag o1, unPgTag o2)

instance (ToStar a, ToJSON b) => ToJSON (PgTagged (a::Symbol) b) where
  toJSON v = object [fromString (T.unpack $ demote @a) .= unPgTag v]

instance (ToStar a, ToJSON b) => ToJSON (PgTagged ('[a]::[Symbol]) b) where
  toJSON v = object [fromString (T.unpack $ demote @a) .= unPgTag v]

instance (ToJSON (PgTagged n r), ToJSON (PgTagged (n1 ': ns) r1))
  => ToJSON (PgTagged (n ': n1 ': ns ::[Symbol]) (r,r1)) where
  toJSON (PgTagged (Tagged (r,r1))) =
    case (toJSON $ pgTag @n r, toJSON $ pgTag @(n1 ': ns) r1) of
      (Object x1, Object x2) -> Object $ x1 <> x2
      _ -> error "PgTagged instances should be always objects"


instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) a) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) a) where
  toField = toJSONField


instance ToStar (Map FstSym0 (TRecordInfo sch t (PgTagged n r))) =>
  CRecordInfo sch t (PgTagged (n::Symbol) r) where
  type TRecordInfo sch t (PgTagged n r) = '[ '( 'FieldInfo n n
    (GetRecField (TTabDef sch t) (TTabRelFrom sch t) (TTabRelTo sch t)
      (TFldDefSym2 sch t) n), r)
    ]

instance
  CRecordInfo sch t (PgTagged n r) => CRecordInfo sch t (PgTagged ('[n]::[Symbol]) r) where
  type TRecordInfo sch t (PgTagged '[n] r) = TRecordInfo sch t (PgTagged n r)

instance
  ( ToStar (Map FstSym0 (TRecordInfo sch t (PgTagged (n ': n1 ':ns) (r,r1))))
  , CRecordInfo sch t (PgTagged n r), CRecordInfo sch t (PgTagged (n1 ':ns) r1))
  => CRecordInfo sch t (PgTagged (n ': n1 ':ns ::[Symbol]) (r,r1)) where
  type TRecordInfo sch t (PgTagged (n ': n1 ':ns) (r,r1)) =
    TRecordInfo sch t (PgTagged n r) ++ TRecordInfo sch t (PgTagged (n1 ': ns) r1)

instance CFieldType sch (PgTagged (n::Symbol) r) n where
  type TFieldType sch (PgTagged n r) n = r

instance CFieldType sch (PgTagged ('[n]::[Symbol]) r) n where
  type TFieldType sch (PgTagged '[n] r) n = r

instance CFieldTypeB (n==x) sch (PgTagged (n ':n1 ':ns) (r,r1)) x
  => CFieldType sch (PgTagged (n ': n1 ': ns ::[Symbol]) (r,r1)) x where
  type TFieldType sch (PgTagged (n ':n1 ':ns) (r,r1)) x=
    TFieldTypeB (n==x) sch (PgTagged (n ':n1 ':ns) (r,r1)) x

class CFieldTypeB (b :: Bool) sch (r :: Type) (n :: Symbol) where
  type TFieldTypeB b sch r n :: Type

instance CFieldTypeB 'True sch (PgTagged (n ':ns) (r,r1)) n where
  type TFieldTypeB 'True sch (PgTagged (n ':ns) (r,r1)) n = r

instance CFieldType sch (PgTagged ns r1) n1
  => CFieldTypeB 'False sch (PgTagged (n ':ns) (r,r1)) n1 where
  type TFieldTypeB 'False sch (PgTagged (n ':ns) (r,r1)) n1 =
    TFieldType sch (PgTagged ns r1) n1

instance
  ( CQueryFields db sch t (TRecordInfo sch t (PgTagged ns r))
  , ToStar (TQueryRecord db sch t (PgTagged ns r)) )
  => CQueryRecord db sch t (PgTagged ns r) where

instance
  ( CSchema sch
  , CDmlFields db sch t (TRecordInfo sch t (PgTagged ns r))
  , ToStar (TDmlRecord db sch t (PgTagged ns r)) )
  => CDmlRecord db sch t (PgTagged ns r) where

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
