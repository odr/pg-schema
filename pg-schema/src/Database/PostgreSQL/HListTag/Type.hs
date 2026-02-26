{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.Type(HListTag(..), HListToJSON(..)) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Kind
import Data.Text as T
import Data.Typeable
import Database.Schema.Def
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Prelude as P

--------------------------------------------------------------------------------
-- 1. DATA STRUCTURE
--------------------------------------------------------------------------------

infixr 5 :*

-- | Heterogeneous list with Symbol tags (field names)
data HListTag (ts :: [(SymNat, Type)]) where
  HNil :: HListTag '[]
  (:*) :: t -> HListTag ts -> HListTag ('(s, t) ': ts)

instance Show (HListTag '[]) where
  show HNil = "HNil"

instance (Show t, KnownSymNat sn, Show (HListTag ts)) => Show (HListTag ('(sn, t) ': ts)) where
  show (x :* xs) = "(" <> P.show x <> ") :* " <> P.show xs

instance Eq (HListTag '[]) where
  _ == _ = True

instance (Eq t, KnownSymNat sn, Eq (HListTag ts)) => Eq (HListTag ('(sn, t) ': ts)) where
  (x :* xs) == (y :* ys) = x == y && xs == ys

--------------------------------------------------------------------------------
-- 2. INSTANCES
--
-- 2.1. Algebraic structures and transformations
--------------------------------------------------------------------------------

instance Semigroup (HListTag '[]) where
  _ <> _ = HNil

instance (Semigroup t, Semigroup (HListTag ts)) => Semigroup (HListTag ('(s, t) ': ts)) where
  (x1 :* xs1) <> (x2 :* xs2) = (x1 <> x2) :* (xs1 <> xs2)

instance Monoid (HListTag '[]) where
  mempty = HNil

instance (Monoid t, Monoid (HListTag ts)) => Monoid (HListTag ('(s, t) ': ts)) where
  mempty = mempty :* mempty

--------------------------------------------------------------------------------
-- 2.2. Database instances
--------------------------------------------------------------------------------
instance ToRow (HListTag '[]) where
  toRow _ = []

instance (ToField t, ToRow (HListTag ts)) => ToRow (HListTag ('(s, t) ': ts)) where
  toRow (val :* xs) = toField val : toRow xs

instance FromRow (HListTag '[]) where
  fromRow = pure HNil

instance (FromField t, FromRow (HListTag ts)) => FromRow (HListTag ('(s, t) ': ts)) where
  fromRow = (:*) <$> field <*> fromRow

instance (FromJSON (HListTag xs), Typeable (HListTag xs)) => FromField (HListTag xs) where
  fromField = fromJSONField

instance (ToJSON (HListTag xs), Typeable (HListTag xs)) => ToField (HListTag xs) where
  toField = toJSONField
--------------------------------------------------------------------------------
-- 2.3. JSON instances
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- $json
--
-- Round-trip and decode examples:
--
-- >>> type SimpleRec = HListTag '[ '( '("b", 0), Bool), '( '("a", 0), Int) ]
-- >>> let val = PgTag False :* PgTag (1::Int) :* HNil :: SimpleRec
-- >>> encode val
-- "{\"b\":false,\"a\":1}"
--
-- >>> decode "{\"a\":10,\"b\":true}" :: Maybe SimpleRec
-- Just (b =: True) :* (a =: 10) :* HNil
--
-- >>> decode "{\"a\":10}" :: Maybe SimpleRec
-- Nothing
--
-- Extra keys in JSON are allowed when decoding:
--
-- >>> decode "{\"a\":1,\"b\":false,\"extra\":42}" :: Maybe SimpleRec
-- Just (b =: False) :* (a =: 1) :* HNil
--
-- Класс-помощник для сериализации и десериализации полей
class HListToJSON ts where
  toSeriesFields :: HListTag ts -> Series
  toMapFields    :: HListTag ts -> KM.KeyMap Value
  parseFields    :: KM.KeyMap Value -> Parser (HListTag ts)

instance HListToJSON '[] where
  toSeriesFields HNil = mempty
  toMapFields    HNil = KM.empty
  parseFields    _    = pure HNil

instance (KnownSymNat sn, ToJSON t, FromJSON t, HListToJSON ts)
      => HListToJSON ('(sn, t) ': ts) where

  toSeriesFields (val :* rest) =
    Key.fromString (T.unpack $ nameSymNat sn) .= val <> toSeriesFields rest

  toMapFields (val :* rest) =
    KM.insert (Key.fromString (T.unpack $ nameSymNat sn)) (toJSON val) (toMapFields rest)

  parseFields km = do
    case KM.lookup key km of
      Nothing -> fail $ "HListTag: missing key " ++ P.show keyString
      Just v  -> do
        -- Парсим значение и, если падает, добавляем контекст с именем поля
        val  <- parseJSON v <?> Key key
        rest <- parseFields km
        pure (val :* rest)
    where
      keyString = T.unpack $ nameSymNat sn
      key = Key.fromString keyString

-- Финальные инстансы
instance HListToJSON ts => ToJSON (HListTag ts) where
  toEncoding hlist = pairs (toSeriesFields hlist)
  toJSON     hlist = Object (toMapFields hlist)

instance HListToJSON ts => FromJSON (HListTag ts) where
  parseJSON = withObject "HListTag" $ \obj -> parseFields obj
