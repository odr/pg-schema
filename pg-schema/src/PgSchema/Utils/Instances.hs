{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PgSchema.Utils.Instances where

import Data.Kind (Type)
import Data.Singletons
import Data.Text as T
import GHC.TypeLits


-- * Bool singletons (avoid "Data.Bool.Singletons" from singletons-base)

data SBool :: Bool -> Type where
  SFalse :: SBool 'False
  STrue :: SBool 'True

type instance Sing @Bool = SBool

instance SingKind Bool where
  type Demote Bool = Bool
  fromSing SFalse = False
  fromSing STrue = True
  toSing False = SomeSing SFalse
  toSing True = SomeSing STrue

instance SingI 'False where sing = SFalse

instance SingI 'True where sing = STrue

-- * Maybe singletons (avoid "Data.Maybe.Singletons" from singletons-base)

data SMaybe :: Maybe k -> Type where
  SNothing :: SMaybe 'Nothing
  SJust :: Sing x -> SMaybe ('Just x)

type instance Sing @(Maybe k) = SMaybe

instance SingKind k => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe (Demote k)
  fromSing SNothing = Nothing
  fromSing (SJust sx) = Just (fromSing sx)
  toSing Nothing = SomeSing SNothing
  toSing (Just x) = case toSing x of
    SomeSing sx -> SomeSing (SJust sx)

instance SingI 'Nothing where
  sing = SNothing

instance SingI x => SingI ('Just x) where
  sing = SJust sing

-- * @Symbol@ singletons (@Demote Symbol = Text@, as in @pg-schema@)

type instance Sing @Symbol = SSymbol

instance SingKind Symbol where
  type Demote Symbol = Text
  fromSing (SSymbol :: SSymbol n) = T.pack (symbolVal (Proxy @n))
  toSing t = case someSymbolVal (T.unpack t) of
    SomeSymbol (_ :: Proxy n) -> SomeSing (SSymbol @n)

instance KnownSymbol n => SingI n where
  sing = SSymbol

-- * List singletons

data SList :: [k] -> Type where
  SNil :: SList '[]
  SCons :: Sing x -> Sing xs -> SList (x ': xs)

type instance Sing @[k] = SList

instance SingKind k => SingKind [k] where
  type Demote [k] = [Demote k]
  fromSing SNil = []
  fromSing (SCons sx sxs) = fromSing sx : fromSing sxs
  toSing [] = SomeSing SNil
  toSing (x : xs) = case (toSing x, toSing xs) of
    (SomeSing sx, SomeSing sxs) -> SomeSing (SCons sx sxs)

instance SingI '[] where
  sing = SNil

instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons sing sing

-- * Pair singletons

data STuple2 :: (a, b) -> Type where
  STuple2 :: Sing x -> Sing y -> STuple2 '(x, y)

type instance Sing @(a, b) = STuple2

instance (SingKind a, SingKind b) => SingKind (a, b) where
  type Demote (a, b) = (Demote a, Demote b)
  fromSing (STuple2 sx sy) = (fromSing sx, fromSing sy)
  toSing (x, y) = case (toSing x, toSing y) of
    (SomeSing sx, SomeSing sy) -> SomeSing (STuple2 sx sy)

instance (SingI a, SingI b) => SingI '(a, b) where
  sing = STuple2 sing sing
