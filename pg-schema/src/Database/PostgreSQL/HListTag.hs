{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag where

import Data.Aeson
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import Data.Aeson.Decoding.Tokens (Tokens (..), TkRecord (..), TkArray (..), Lit (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as BL
import Control.Applicative (Alternative (..))
import Data.Kind (Type)
import Data.Proxy
import Database.PostgreSQL.PgTagged
import Database.Types.SchList (SchList (..))
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

infixr 5 :*

-- | Heterogeneous list with Symbol tags (field names)
data HListTag (ts :: [(SymNat, Type)]) where
  HNil :: HListTag '[]
  (:*) :: PgTagged s t -> HListTag ts -> HListTag ('(s, t) ': ts)

instance Show (HListTag '[]) where
  show HNil = "HNil"

instance (Show t, KnownSymNat sn s n, Show (HListTag ts)) => Show (HListTag ('(sn, t) ': ts)) where
  show (x :* xs) = "(" <> show x <> ") :* " <> show xs

instance Eq (HListTag '[]) where
  _ == _ = True

instance (Eq t, KnownSymNat sn s n, Eq (HListTag ts)) => Eq (HListTag ('(sn, t) ': ts)) where
  (x :* xs) == (y :* ys) = x == y && xs == ys

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

--------------------------------------------------------------------------------
-- 2.4. Streaming decoding from JSON tokens
--------------------------------------------------------------------------------

-- $streaming
--
-- Stream decoding avoids building an intermediate 'Data.Aeson.Types.Object':
--
-- >>> let bs = encode (object ["b" .= False, "a" .= (1::Int)])
-- >>> streamDecodeHListTag @('[ '( '("b", 0), Bool), '( '("a", 0), Int) ]) bs
-- Right (b =: False) :* (a =: 1) :* HNil
--
-- The primed variant also returns the leftover input:
--
-- >>> let (Right (h, rest)) = streamDecodeHListTag' @('[ '( '("a", 0), Int), '( '("b", 0), Bool) ]) bs
-- >>> BL.null rest
-- True
--
-- | Generic initialization of an 'HListTag' lifted into an 'Alternative'
-- functor @f@ (e.g. @Maybe@). All slots are set to 'empty'.
class HEmpty ts f where
  hEmpty :: HListTag (Lifted ts f)

instance Alternative f => HEmpty '[] f where
  hEmpty = HNil

instance (Alternative f, HEmpty ts f) => HEmpty ('(s, t) ': ts) f where
  hEmpty = PgTag empty :* hEmpty @ts @f

-- | Rank-2 \"sequence\" for 'HListTag': collapse an extra 'Applicative'
-- layer around all fields.
class HUnLift ts f where
  hUnLift :: Applicative f => HListTag (Lifted ts f) -> f (HListTag ts)

instance Applicative f => HUnLift '[] f where
  hUnLift HNil = pure HNil

instance (Applicative f, HUnLift ts f) => HUnLift ('(s, t) ': ts) f where
  hUnLift (PgTag ft :* rest) =
    (:*) . PgTag <$> ft <*> hUnLift rest

-- | Update a lifted 'HListTag' by JSON key: for the matching field name,
-- parse the given 'Value' into the field's type and store it in @f@.
class HUpdateByKey ts f where
  hUpdateByKey
    :: Applicative f
    => Key.Key
    -> Value
    -> HListTag (Lifted ts f)
    -> Either String (HListTag (Lifted ts f))

instance Applicative f => HUpdateByKey '[] f where
  hUpdateByKey _ _ HNil = Right HNil

instance
  ( KnownSymNat sn s n
  , FromJSON t
  , Applicative f
  , HUpdateByKey ts f
  ) => HUpdateByKey ('(sn, t) ': ts) f where
  hUpdateByKey key v (PgTag ft :* rest)
    | key == expectedKey = do
        x <- firstPrefix "HListTag field parse error: " (parseEither parseJSON v)
        pure (PgTag (pure x) :* rest)
    | otherwise = do
        rest' <- hUpdateByKey @ts @f key v rest
        pure (PgTag ft :* rest')
    where
      expectedKey = Key.fromString (nameSymNat sn)

      firstPrefix :: String -> Either String a -> Either String a
      firstPrefix _ (Right a) = Right a
      firstPrefix pfx (Left e) = Left (pfx <> e)

-- | Parse a value of type @t@ from a JSON token stream (one value).
-- Returns the value and the continuation (rest of the stream after the value).
-- Used for streaming decode of field values without building an intermediate 'Value'.
class ParseFromTokens t where
  parseFromTokens :: Tokens k String -> Either String (t, k)

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (HListTag ts) where
  parseFromTokens (TkRecordOpen rec) = parseFromRecord @ts rec
  parseFromTokens _ = Left "HListTag: expected JSON object"

parseArray
  :: forall ts k.
     (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => TkArray k String
  -> Either String (SchList (HListTag ts), k)
parseArray = go []
  where
    go _ (TkArrayErr e) = Left e
    go acc (TkArrayEnd k) = Right (SchList (reverse acc), k)
    go acc (TkItem toks) = do
      (h, arr') <- parseFromTokens @(HListTag ts) toks
      go (h : acc) arr'

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (SchList (HListTag ts)) where
  parseFromTokens (TkArrayOpen arr) = parseArray @ts arr
  parseFromTokens _ = Left "SchList HListTag: expected JSON array"

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (Maybe (HListTag ts)) where
  parseFromTokens (TkLit LitNull k) = Right (Nothing, k)
  parseFromTokens (TkRecordOpen rec) = do
    (h, k) <- parseFromRecord @ts rec
    pure (Just h, k)
  parseFromTokens _ = Left "Maybe HListTag: expected null or JSON object"

instance {-# OVERLAPPABLE #-} FromJSON t => ParseFromTokens t where
  parseFromTokens toks = do
    (v, k) <- toEitherValue toks
    x <- firstPrefix "ParseFromTokens: " (parseEither parseJSON v)
    pure (x, k)
   where
    firstPrefix _ (Right a) = Right a
    firstPrefix pfx (Left e) = Left (pfx <> e)

-- | Update a lifted 'HListTag' by JSON key using the token stream of the value.
-- When the key matches the current field, parses from tokens (streaming for
-- HListTag/SchList/Maybe); when it does not, skips the value and recurses.
class HUpdateByKeyStream ts f where
  hUpdateByKeyStream
    :: Applicative f
    => Key.Key
    -> Tokens (TkRecord k String) String
    -> HListTag (Lifted ts f)
    -> Either String (HListTag (Lifted ts f), TkRecord k String)

instance Applicative f => HUpdateByKeyStream '[] f where
  hUpdateByKeyStream _key toks HNil = do
    (_, rec') <- toEitherValue toks
    pure (HNil, rec')

instance
  ( KnownSymNat sn s n
  , ParseFromTokens t
  , Applicative f
  , HUpdateByKeyStream ts f
  ) => HUpdateByKeyStream ('(sn, t) ': ts) f where
  hUpdateByKeyStream key toks (PgTag ft :* rest)
    | key == expectedKey = do
        (x, rec') <- parseFromTokens @t toks
        pure (PgTag (pure x) :* rest, rec')
    | otherwise = do
        (rest', rec'') <- hUpdateByKeyStream @ts @f key toks rest
        pure (PgTag ft :* rest', rec'')
   where
    expectedKey = Key.fromString (nameSymNat sn)

-- | Internal: parse an 'HListTag ts' from a JSON object token stream.
-- The result also returns the leftover continuation @k@ after the object.
parseFromRecord
  :: forall ts k. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => TkRecord k String
  -> Either String (HListTag ts, k)
parseFromRecord = go (hEmpty @ts @Maybe)
  where
    go
      :: HListTag (Lifted ts Maybe)
      -> TkRecord k String
      -> Either String (HListTag ts, k)
    go acc rec = case rec of
      TkRecordErr e -> Left e
      TkRecordEnd k ->
        case hUnLift @ts @Maybe acc of
          Nothing -> Left "HListTag: missing required field(s)"
          Just h  -> Right (h, k)
      TkPair key toks -> do
        (acc', rec') <- hUpdateByKeyStream @ts @Maybe key toks acc
        go acc' rec'

-- | Streamingly decode an 'HListTag' from a lazy 'ByteString', returning
-- both the decoded value and the leftover input.
streamDecodeHListTag'
  :: forall ts. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => BL.ByteString
  -> Either String (HListTag ts, BL.ByteString)
streamDecodeHListTag' bs =
  case lbsToTokens bs of
    TkRecordOpen rec ->
      case parseFromRecord @ts rec of
        Left e          -> Left e
        Right (h, rest) -> Right (h, rest)
    TkErr e -> Left e
    _       -> Left "HListTag: expected top-level JSON object"

-- | Streamingly decode an 'HListTag' from a lazy 'ByteString'.
streamDecodeHListTag
  :: forall ts. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => BL.ByteString
  -> Either String (HListTag ts)
streamDecodeHListTag bs = fmap fst (streamDecodeHListTag' @ts bs)

--------------------------------------------------------------------------------
-- 3. RENAMING ENGINE (Defunctionalization)
--------------------------------------------------------------------------------

-- | Class for type-level string transformation "functions"
class Renamer r where
  type Apply r (s :: Symbol) :: Symbol

-- | Identity renamer: keeps field names as is
data RenamerId
instance Renamer RenamerId where
  type Apply RenamerId s = s
