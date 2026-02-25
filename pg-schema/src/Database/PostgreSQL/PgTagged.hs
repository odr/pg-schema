{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Database.PostgreSQL.PgTagged where

import Data.Coerce
#ifdef MK_HASHABLE
import Data.Hashable
#endif
import Data.String
import Data.Tagged
import Data.Text as T
import Database.Schema.Def
import GHC.TypeLits as TL
import Prelude as P
import Prelude.Singletons
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat as F
#endif


newtype PgTagged a b = PgTagged (Tagged a b)
  -- deriving stock (Read, Show)
  deriving newtype
  ( Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup, Num, Real, Integral, Enum, Bounded, RealFloat, RealFrac, Floating
  , Fractional, IsString )

instance (KnownSymbol s, Show b) => Show (PgTagged (s :: Symbol) b)  where
  show (PgTag v) = symbolVal (Proxy @s) <> " =: " <> P.show v

instance (KnownSymNat sn, Show b)
  => Show (PgTagged (sn :: SymNat) b)  where
    show (PgTag v) = T.unpack (nameSymNat sn) <> " =: " <> P.show v

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
