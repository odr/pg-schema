{-# LANGUAGE CPP #-}
module Util.ToStar where

-- import Data.Kind (Type)
-- import Data.List as L
-- import Data.Proxy (Proxy(..))
-- import Data.Text as T
-- import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
-- import Type.Reflection (Typeable, typeRep)
import Data.Kind
import Data.Singletons.Prelude


type family ToStar (a::k) :: Constraint where
  ToStar (a::k) = (SingKind k, SingI a)

toStar :: forall k (a::k). ToStar (a::k) => Demote k
#if MIN_VERSION_base(4,12,0)
toStar = fromSing @k (sing @a)
#else
toStar = fromSing @k (sing @_ @a)
#endif

-- class (SingKind k, SingI a) => ToStar (a::k) where
--   toStar :: Demote k
--
-- instance (SingKind k, SingI a) => ToStar (a::k) where
--   toStar = fromSing @k (sing @_ @a)


--
-- type family TStar k :: Type
--
-- type instance TStar Symbol = T.Text
-- type instance TStar Bool  = Bool
-- type instance TStar [k] = [TStar k]
-- type instance TStar (k1,k2) = (TStar k1, TStar k2)
-- type instance TStar (Maybe k) = Maybe (TStar k)
--
-- class ToStar (a::k) where
--   toStar :: TStar k
--
-- instance KnownSymbol s => ToStar (s::Symbol) where
--   toStar = T.pack $ symbolVal $ Proxy @s
--
-- instance Typeable s => ToStar (s::Bool) where
--   toStar = read $ L.tail $ show $ typeRep @s
--
-- instance ToStar '[] where
--   toStar = []
--
-- instance (ToStar x, ToStar xs)
--       => ToStar (x ': xs) where
--   toStar = toStar @_ @x : toStar @_ @xs
--
-- instance (ToStar x, ToStar y) => ToStar '(x,y) where
--   toStar = (toStar @_ @x, toStar @_ @y)
--
-- instance ToStar x => ToStar ('Just x) where
--   toStar = Just (toStar @_ @x)
--
-- instance ToStar 'Nothing where
--   toStar = Nothing

-- type family Demote2 (t::kt) k :: Type
--
-- type instance Demote2 t [k] = [Demote2 t k]
-- type instance Demote2 t (k1,k2) = (Demote2 t k1,Demote2 t k2)
-- type instance Demote2 t (Maybe k) = Maybe (Demote2 t k)
--
-- class CDemote2 t (fnames :: k) where
--   de2 :: Demote2 t k
--
-- instance CDemote2 t ('[] :: [k]) where
--   de2 = []
--
-- instance (CDemote2 t (x::k), CDemote2 t (xs::[k])) => CDemote2 t (x ': xs) where
--   de2 = de2 @t @x : de2 @t @xs
--
-- type family Cs (cs :: [Constraint]) :: Constraint where
--   Cs '[] = ()
--   Cs (c ': cs) = (c, Cs cs)
