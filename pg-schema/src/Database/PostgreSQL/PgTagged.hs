{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Database.PostgreSQL.PgTagged where

import Data.Aeson
import Data.Coerce
#ifdef MK_HASHABLE
import Data.Hashable
#endif
import Data.String
import Data.Tagged
import Data.Text as T
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.ToRow as PG
import GHC.TypeLits as TL
import PgSchema.Util
import Prelude as P
import Prelude.Singletons
import Type.Reflection
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat as F
#endif


newtype PgTagged (a :: Symbol) b = PgTagged (Tagged a b)
  deriving newtype
  ( Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup, Num, Real, Integral, Enum, Bounded, RealFloat, RealFrac, Floating
  , Fractional, IsString )

instance (KnownSymbol a, Show b) => Show (PgTagged a b) where
  show (PgTag b) = "\"" <> symbolVal (Proxy @a) <> "\" =: (" <> P.show b <> ")"

#ifdef MK_ARBITRARY
instance Arbitrary b => Arbitrary (PgTagged a b) where
  arbitrary = PgTag <$> arbitrary
#endif

#ifdef MK_FLAT
instance Flat b => Flat (PgTagged a b) where
  encode = F.encode . unPgTag
  decode = PgTag <$> F.decode
  size = F.size . unPgTag

#endif

#ifdef MK_HASHABLE
instance Hashable b => Hashable (PgTagged a b) where
  hashWithSalt s = hashWithSalt @b s . coerce
#endif

{-# COMPLETE PgTag #-}
pattern PgTag :: forall a b. b -> PgTagged a b
pattern PgTag b = PgTagged (Tagged b)

(=:) :: forall b. forall a -> b -> PgTagged a b
(=:) _ = coerce
infixr 5 =:

type s := t = PgTagged s t

unPgTag :: forall a b. PgTagged a b -> b
unPgTag = coerce

instance (ToStar a, FromJSON b) => FromJSON (a := b) where
  parseJSON = withObject "PgTagged " \v ->
    PgTag <$> v .: fromString (T.unpack $ demote @a)

instance (ToStar a, ToJSON b) => ToJSON (a := b) where
  toJSON v = object [fromString (T.unpack $ demote @a) .= unPgTag v]

-- used in child (SchList) fields
instance (FromJSON a, Typeable a, KnownSymbol n) => FromField (n := a) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (n := a) where
  toField = toJSONField

instance FromRow (Only b) => FromRow (n := b) where
  fromRow = coerce @(Only b) <$> fromRow

instance ToRow (Only b) => ToRow (n := b) where
  toRow = toRow @(Only b) . coerce
