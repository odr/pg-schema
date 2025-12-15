{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Database.PostgreSQL.PgTagged where

import Data.Aeson
import Data.Coerce
import Data.Hashable
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
import Database.Types.SchList (SchList)
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat as F
#endif


newtype PgTagged a b = PgTagged (Tagged a b)
  deriving stock (Read, Show)
  deriving newtype
  ( Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup, Num, Real, Integral, Enum, Bounded, RealFloat, RealFrac, Floating
  , Fractional, IsString )

#ifdef MK_ARBITRARY
instance Arbitrary b => Arbitrary (PgTagged a b) where
  arbitrary = pgTag <$> arbitrary
#endif

#ifdef MK_FLAT
instance Flat b => Flat (PgTagged a b) where
  encode = F.encode . unPgTag
  decode = pgTag <$> F.decode
  size = F.size . unPgTag

#endif

instance Hashable b => Hashable (PgTagged a b) where
  hashWithSalt s = hashWithSalt @b s . coerce

{-# COMPLETE PgTag #-}
pattern PgTag :: forall a b. b -> PgTagged a b
pattern PgTag b = PgTagged (Tagged b)

(=:) :: forall b. forall a -> b -> PgTagged a b
(=:) _ = coerce
infixr 5 =:

-- class PgTaggedAppend a b as bs as' bs' | a as -> as', b bs -> bs' where
--   (<>:) :: PgTagged a b -> PgTagged as bs -> PgTagged as' bs'

-- infixr 1 <>:

-- instance PgTaggedAppend (a :: k) b (as :: [k]) bs (a : as) (b, bs) where
--   PgTag x <>: PgTag xs = PgTag (x, xs)

-- instance PgTaggedAppend (a :: k) b (as :: k) bs [a, as] (b, bs) where
--   PgTag x <>: PgTag xs = PgTag (x, xs)

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

-- 'Binary a' can't be converted to/from JSON so we support it only
-- on the top level
instance
  (FromField (Binary a), Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) (Binary a)) where
  fromField f = fmap (PgTag @n) . fromField f

instance (ToField (Binary a), ToStar n) => ToField (PgTagged (n::Symbol) (Binary a)) where
  toField = toField . unPgTag

instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) a) where
  fromField = fromJSONField

-- For non-Binary we convert it through JSON
instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) a) where
  toField = toJSONField

instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) (SchList a)) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) (SchList a)) where
  toField = toJSONField

instance (MkRecField sch (FieldKind (Fst (Head (TRecordInfo sch t (PgTagged n r))))) r
  , ToStar n, ToStar t) =>
  CRecordInfo sch t (PgTagged (n::Symbol) r) where
  type TRecordInfo sch t (PgTagged n r) = '[ '( 'FieldInfo n n
    (GetRecField t (TTabDef sch t) (TTabRelFrom sch t) (TTabRelTo sch t)
      (TFldDefSym1 sch) n), r)
    ]
  getRecordInfo = RecordInfo (demote @t) [FieldInfo (demote @n) (demote @n)
    $ mkRecField @sch @(FieldKind (Fst (Head (TRecordInfo sch t (PgTagged n r))))) @r]

instance
  CRecordInfo sch t (PgTagged n r) => CRecordInfo sch t (PgTagged ('[n]::[Symbol]) r) where
  type TRecordInfo sch t (PgTagged '[n] r) = TRecordInfo sch t (PgTagged n r)
  getRecordInfo = getRecordInfo @sch @t @(PgTagged n r)

instance
  ( CRecordInfo sch t (PgTagged n r), CRecordInfo sch t (PgTagged (n1 ':ns) r1))
  => CRecordInfo sch t (PgTagged (n ': n1 ':ns ::[Symbol]) (r,r1)) where
  type TRecordInfo sch t (PgTagged (n ': n1 ':ns) (r,r1)) =
    TRecordInfo sch t (PgTagged n r) ++ TRecordInfo sch t (PgTagged (n1 ': ns) r1)
  getRecordInfo = let r1 = getRecordInfo @sch @t @(PgTagged n r) in r1
    { fields = r1.fields
      <> (getRecordInfo @sch @t @(PgTagged (n1 ': ns) r1)).fields }

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

-- data SomeFieldVal sch t where
--   SomeFieldVal
--     :: forall fld v sch tab. CRecordInfo sch tab (PgTagged fld v)
--     => PgTagged fld v -> SomeFieldVal sch tab

-- (=:) :: forall sch t v. forall fld ->
--   CRecordInfo sch t (PgTagged fld v) => v -> SomeFieldVal sch t
-- fld =: val = SomeFieldVal (pgTag @fld val)

-- instance CRecordInfo sch t (SomeFieldVal sch t) where
--   type TRecordInfo sch t (SomeFieldVal sch t)
