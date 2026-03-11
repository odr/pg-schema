{-# LANGUAGE UndecidableInstances #-}
module PgSchema.HList.Type(HList(..)) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Kind
import Data.Text as T
import Data.Typeable
import PgSchema.Schema
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
--
-- All needed instances (JSON | Field | Row) are there
-- so no need of them for user's types if they have t'PgSchema.HList.IsoHList' instance
data HList (ts :: [(SymNat, Type)]) where
  HNil :: HList '[]
  (:*) :: t -> HList ts -> HList ('(s, t) ': ts)

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, KnownSymNat sn, Show (HList ts)) => Show (HList ('(sn, t) ': ts)) where
  show (x :* xs) = "(" <> P.show x <> ") :* " <> P.show xs

instance Eq (HList '[]) where
  _ == _ = True

instance (Eq t, KnownSymNat sn, Eq (HList ts)) => Eq (HList ('(sn, t) ': ts)) where
  (x :* xs) == (y :* ys) = x == y && xs == ys

--------------------------------------------------------------------------------
-- 2. INSTANCES
--
-- 2.1. Algebraic structures and transformations
--------------------------------------------------------------------------------

instance Semigroup (HList '[]) where
  _ <> _ = HNil

instance (Semigroup t, Semigroup (HList ts)) => Semigroup (HList ('(s, t) ': ts)) where
  (x1 :* xs1) <> (x2 :* xs2) = (x1 <> x2) :* (xs1 <> xs2)

instance Monoid (HList '[]) where
  mempty = HNil

instance (Monoid t, Monoid (HList ts)) => Monoid (HList ('(s, t) ': ts)) where
  mempty = mempty :* mempty

--------------------------------------------------------------------------------
-- 2.2. Database instances
--------------------------------------------------------------------------------
instance ToRow (HList '[]) where
  toRow _ = []
instance (ToField t, ToRow (HList ts)) => ToRow (HList ('(s, t) ': ts)) where
  toRow (val :* xs) = toField val : toRow xs

instance FromRow (HList '[]) where
  fromRow = pure HNil

instance (FromField t, FromRow (HList ts)) => FromRow (HList ('(s, t) ': ts)) where
  fromRow = (:*) <$> field <*> fromRow

instance (FromJSON (HList xs), Typeable (HList xs)) => FromField (HList xs) where
  fromField = fromJSONField

instance (ToJSON (HList xs), Typeable (HList xs)) => ToField (HList xs) where
  toField = toJSONField

instance (FromJSON (HList xs), Typeable (HList xs)) => FromField [HList xs] where
  fromField = fromJSONField

instance (ToJSON (HList xs), Typeable (HList xs)) => ToField [HList xs] where
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
-- >>> type SimpleRec = HList '[ '( '("b", 0), Bool), '( '("a", 0), Int) ]
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
class HListToJSON ts where
  toSeriesFields :: HList ts -> Series
  toMapFields    :: HList ts -> KM.KeyMap Value
  parseFields    :: KM.KeyMap Value -> Parser (HList ts)

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
      Nothing -> fail $ "HList: missing key " ++ P.show keyString
      Just v  -> do
        val  <- parseJSON v <?> Key key
        rest <- parseFields km
        pure (val :* rest)
    where
      keyString = T.unpack $ nameSymNat sn
      key = Key.fromString keyString

instance HListToJSON ts => ToJSON (HList ts) where
  toEncoding hlist = pairs (toSeriesFields hlist)
  toJSON     hlist = Object (toMapFields hlist)

instance HListToJSON ts => FromJSON (HList ts) where
  parseJSON = withObject "HList" $ \obj -> parseFields obj
