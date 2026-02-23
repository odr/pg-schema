{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Kind (Type)
import Data.Proxy
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.TypeLits


-- | Type-level append for list of types.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

--------------------------------------------------------------------------------
-- 1. DATA STRUCTURE
--------------------------------------------------------------------------------

infixr 3 :*

-- | Heterogeneous list with Symbol tags (field names)
data HListTag (ts :: [(SymNat, Type)]) where
  HNil :: HListTag '[]
  (:*) :: PgTagged s t -> HListTag ts -> HListTag ('(s, t) ': ts)

instance Show (HListTag '[]) where
  show HNil = "HNil"

instance (Show t, KnownSymNat sn s n, Show (HListTag ts)) => Show (HListTag ('(sn, t) ': ts)) where
  show (x :* xs) = show x <> " :* " <> show xs

--------------------------------------------------------------------------------
-- 2. INSTANCES
--
-- 2.1. Algebraic structures and transformations
--------------------------------------------------------------------------------

instance Semigroup (HListTag '[]) where
  _ <> _ = HNil

instance (Semigroup t, Semigroup (HListTag ts)) => Semigroup (HListTag ('(s, t) ': ts)) where
  (PgTag x1 :* xs1) <> (PgTag x2 :* xs2) =
    PgTag (x1 <> x2) :* (xs1 <> xs2)

instance Monoid (HListTag '[]) where
  mempty = HNil

instance (Monoid t, Monoid (HListTag ts)) => Monoid (HListTag ('(s, t) ': ts)) where
  mempty = PgTag mempty :* mempty

-- | Lifting values into an Applicative/Functor structure
class HLift (ts :: [(SymNat, Type)]) where
  hlift :: (forall x. x -> f x)
        -> HListTag ts
        -> HListTag (Lifted ts f)

type family Lifted (ts :: [(SymNat, Type)]) (f :: Type -> Type) :: [(SymNat, Type)] where
  Lifted '[] f = '[]
  Lifted ('(s, t) ': ts) f = '(s, f t) ': Lifted ts f

instance HLift '[] where
  hlift _ HNil = HNil

instance HLift ts => HLift ('(s, t) ': ts) where
  hlift f (PgTag x :* xs) = PgTag (f x) :* hlift f xs

--------------------------------------------------------------------------------
-- 2.2. Database instances
--------------------------------------------------------------------------------
instance ToRow (HListTag '[]) where
  toRow _ = []

instance (ToField t, ToRow (HListTag ts)) => ToRow (HListTag ('(s, t) ': ts)) where
  toRow (PgTag val :* xs) = toField val : toRow xs

instance FromRow (HListTag '[]) where
  fromRow = pure HNil

instance (FromField t, FromRow (HListTag ts)) => FromRow (HListTag ('(s, t) ': ts)) where
  fromRow = ((:*) . PgTag <$> field) <*> fromRow

--------------------------------------------------------------------------------
-- 2.3. JSON instances
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- Класс-помощник для сериализации и десериализации полей
class HListToJSON ts where
  toSeriesFields :: HListTag ts -> Series
  toMapFields    :: HListTag ts -> KM.KeyMap Value
  parseFields    :: KM.KeyMap Value -> Parser (HListTag ts)

instance HListToJSON '[] where
  toSeriesFields HNil = mempty
  toMapFields    HNil = KM.empty
  parseFields    _    = pure HNil

instance (KnownSymNat sn s n, ToJSON t, FromJSON t, HListToJSON ts)
      => HListToJSON ('(sn, t) ': ts) where

  toSeriesFields (PgTagged val :* rest) =
    Key.fromString (nameSymNat sn) .= val <> toSeriesFields rest

  toMapFields (PgTagged val :* rest) =
    KM.insert (Key.fromString $ symbolVal (Proxy @s)) (toJSON val) (toMapFields rest)

  parseFields km = do
    case KM.lookup key km of
      Nothing -> fail $ "HListTag: missing key " ++ show keyString
      Just v  -> do
        -- Парсим значение и, если падает, добавляем контекст с именем поля
        val  <- parseJSON v <?> Key key
        rest <- parseFields km
        pure (PgTagged val :* rest)
    where
      keyString = nameSymNat sn
      key = Key.fromString keyString

-- Финальные инстансы
instance HListToJSON ts => ToJSON (HListTag ts) where
  toEncoding hlist = pairs (toSeriesFields hlist)
  toJSON     hlist = Object (toMapFields hlist)

instance HListToJSON ts => FromJSON (HListTag ts) where
  parseJSON = withObject "HListTag" $ \obj -> parseFields obj
