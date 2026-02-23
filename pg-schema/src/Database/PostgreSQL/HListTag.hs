{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Kind (Type, Constraint)
import Data.Proxy
import Database.PostgreSQL.PgProduct
import Database.PostgreSQL.PgTagged
import Database.Types.EmptyField (EmptyField, emptyField)
import Database.Types.SchList (SchList (..))
import Database.PostgreSQL.Simple.FromField hiding (Text)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.Schema.Def
  ( CFieldInfo (..), HasNullableRefs, TFieldInfo
  , RecFieldK, NameNSK, RecField'(RFPlain, RFFromHere, RFToHere, RFAggr) )
import Database.Schema.Rec (IsMaybe, UnMaybe)
import GHC.Generics (Generic (..), Rep, M1 (..), K1 (..), (:*:)(..), U1 (..), D1, C1, S, R, Meta (..))
import GHC.TypeLits
import GHC.TypeError (Assert)


-- | Type-level append for list of types.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Append for HListTag list (SymNat, Type).
type family AppendHList (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  AppendHList '[] ys = ys
  AppendHList (x ': xs) ys = x ': AppendHList xs ys

-- | Symbol part of SymNat.
type family SymNatSymbol (sn :: SymNat) :: Symbol where
  SymNatSymbol '(s, n) = s

-- | Add 1 to n when symbols equal (for renumbering).
type family AddIfSymEq (a :: Symbol) (b :: Symbol) (n :: Nat) :: Nat where
  AddIfSymEq s s n = 1 + n
  AddIfSymEq s s' n = n

-- | Count how many keys in list have the given symbol.
type family CountSymIn (acc :: [(SymNat, Type)]) (s :: Symbol) :: Nat where
  CountSymIn '[] s = 0
  CountSymIn ('(sn, _) ': rest) s = AddIfSymEq (SymNatSymbol sn) s (CountSymIn rest s)

-- | Count how many keys in [SymNat] have the given symbol (for NormalizeGo).
type family CountSymInKeys (keys :: [SymNat]) (s :: Symbol) :: Nat where
  CountSymInKeys '[] s = 0
  CountSymInKeys (sn ': rest) s = AddIfSymEq (SymNatSymbol sn) s (CountSymInKeys rest s)

-- | NormalizeGo prefix xs = renumbered tail when prefix = keys emitted so far.
type family NormalizeGo (prefix :: [SymNat]) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  NormalizeGo prefix '[] = '[]
  NormalizeGo prefix ('(sn, t) ': rest) =
    '( '(SymNatSymbol sn, CountSymInKeys prefix (SymNatSymbol sn)), t)
    ': NormalizeGo (sn ': prefix) rest

-- | Normalize: renumber Nats per symbol 0, 1, 2, ...
type family Normalize (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Normalize xs = NormalizeGo '[] xs

-- | Length of HListTag list.
type family Length (xs :: [(SymNat, Type)]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

-- | Take first n elements.
type family Take (n :: Nat) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Take 0 xs = '[]
  Take n '[] = '[]
  Take n (x ': xs) = x ': Take (n - 1) xs

-- | Drop first n elements.
type family Drop (n :: Nat) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Drop 0 xs = xs
  Drop n '[] = '[]
  Drop n (_ ': xs) = Drop (n - 1) xs

-- | Types only (strip keys) for retag.
type family TypesOf (xs :: [(SymNat, Type)]) :: [Type] where
  TypesOf '[] = '[]
  TypesOf ('(_, t) ': xs) = t ': TypesOf xs

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

--------------------------------------------------------------------------------
-- 3.5. HListTag append, normalize, split, retag (for (:..))
--------------------------------------------------------------------------------

class HListAppend (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) where
  appendHListTag :: HListTag xs -> HListTag ys -> HListTag (AppendHList xs ys)

instance HListAppend '[] ys where
  appendHListTag HNil ys = ys

instance HListAppend rest ys => HListAppend ('(sn, t) ': rest) ys where
  appendHListTag (PgTag x :* xs) ys = PgTag x :* appendHListTag xs ys

class NormalizeGoHListTag (prefix :: [SymNat]) (xs :: [(SymNat, Type)]) where
  normalizeGoHListTag :: HListTag xs -> HListTag (NormalizeGo prefix xs)

instance NormalizeGoHListTag prefix '[] where
  normalizeGoHListTag HNil = HNil

instance
  ( NormalizeGoHListTag (sn ': prefix) rest
  , KnownSymNat sn s n
  , s ~ SymNatSymbol sn
  , n' ~ CountSymInKeys prefix s
  , KnownNat n'
  )
  => NormalizeGoHListTag prefix ('(sn, t) ': rest)
  where
  normalizeGoHListTag (PgTag x :* rest) =
    PgTag @'(s, n') x :* normalizeGoHListTag @(sn ': prefix) @rest rest

class NormalizeHListTag (xs :: [(SymNat, Type)]) where
  normalizeHListTag :: HListTag xs -> HListTag (Normalize xs)

instance (out ~ NormalizeGo '[] xs, NormalizeGoHListTag '[] xs) => NormalizeHListTag xs where
  normalizeHListTag = normalizeGoHListTag @'[] @xs

class SplitAtHListTag (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) where
  splitAtHListTag :: HListTag (AppendHList xs ys) -> (HListTag xs, HListTag ys)

instance SplitAtHListTag '[] ys where
  splitAtHListTag h = (HNil, h)

instance SplitAtHListTag rest ys => SplitAtHListTag ('(sn, t) ': rest) ys where
  splitAtHListTag (PgTag x :* tl) =
    let (restH, ysH) = splitAtHListTag tl
    in (PgTag x :* restH, ysH)

class SplitAtHListTagN (n :: Nat) (zs :: [(SymNat, Type)]) where
  splitAtHListTagN :: HListTag zs -> (HListTag (Take n zs), HListTag (Drop n zs))

instance SplitAtHListTagN 0 zs where
  splitAtHListTagN h = (HNil, h)

instance
  ( m ~ (n - 1)
  , SplitAtHListTagN m rest
  , Take n ('(sn, t) ': rest) ~ ('(sn, t) ': Take m rest)
  , Drop n ('(sn, t) ': rest) ~ Drop m rest
  )
  => SplitAtHListTagN n ('(sn, t) ': rest)
  where
  splitAtHListTagN (PgTag x :* tl) =
    let (a, b) = splitAtHListTagN @m @rest tl
    in (PgTag x :* a, b)

class RetagHListTag (a :: [(SymNat, Type)]) (b :: [(SymNat, Type)]) where
  retagHListTag :: HListTag a -> HListTag b

instance RetagHListTag '[] '[] where
  retagHListTag HNil = HNil

instance (RetagHListTag resta restb, TypesOf a ~ TypesOf b) =>
  RetagHListTag ('(sna, t) ': resta) ('(snb, t) ': restb)
  where
  retagHListTag (PgTag x :* rest) = PgTag @snb x :* retagHListTag rest

--------------------------------------------------------------------------------
-- 4. IsoHListTag: Generic record <-> HListTag
--------------------------------------------------------------------------------

-- | Extract field name symbol from Generic selector metadata (MetaSel).
type family GSelName (m :: Meta) :: Symbol where
  GSelName (MetaSel ('Just s) u v w) = s

-- | Count occurrences of symbol @s@ in HListTag list (for unique Nat in SymNat).
type family CountName (s :: Symbol) (xs :: [(SymNat, Type)]) :: Nat where
  CountName s '[] = 0
  CountName s ('( '(s, _), _) ': xs) = 1 + CountName s xs
  CountName s (_ ': xs) = CountName s xs

-- | Type-level equality for Bool (for Assert).
type family EqBool (a :: Bool) (b :: Bool) :: Bool where
  EqBool a a = 'True
  EqBool a b = 'False

-- | Constraint for RFFromHere: nullable refs must match Maybe type (variant B).
type family RFFromHereNullable refs t :: Constraint where
  RFFromHereNullable refs t =
    Assert (EqBool (HasNullableRefs refs) (IsMaybe t))
      (TypeError ( 'Text "IsoHListTag: RFFromHere field nullable must match type: "
                :<>: 'Text " HasNullableRefs = IsMaybe t"
                :$$: 'Text "References: " :<>: ShowType refs
                :$$: 'Text "User type:" :<>: ShowType t))

-- | HListTag field type from schema field kind and record field type.
type family FieldHListType' (ren :: Type) (sch :: k) (rf :: RecFieldK NameNSK) (t :: Type) :: Type where
  FieldHListType' ren sch ('RFPlain _fd) t = t
  FieldHListType' ren sch ('RFFromHere toTab _refs) t =
    If (IsMaybe t)
       (Maybe (HListTag (HListTagRep ren sch toTab (UnMaybe t))))
       (HListTag (HListTagRep ren sch toTab t))
  FieldHListType' ren sch ('RFToHere fromTab _refs) (SchList child) =
    SchList (HListTag (HListTagRep ren sch fromTab child))
  FieldHListType' ren sch ('RFAggr _ _ _) t = t

type family If (b :: Bool) (t :: Type) (f :: Type) :: Type where
  If 'True t _ = t
  If 'False _ f = f

-- | HListTag field type for (sch, tab, name) and record field type t.
type family FieldHListType ren sch tab (sym :: Symbol) (t :: Type) :: Type where
  FieldHListType ren sch tab sym t =
    FieldHListType' ren sch (TFieldInfo sch tab sym) t

-- | Add one field to HListTag list (renamed name + Nat for uniqueness).
type family AddOneField ren sch tab (sym :: Symbol) (t :: Type) (acc :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  AddOneField ren sch tab sym t acc =
    '( '( Apply ren sym, CountName (Apply ren sym) acc )
     , FieldHListType ren sch tab sym t
     ) ': acc

-- | Build HListTag list from Generic product (:*:).
type family GFieldsToHList ren sch tab (f :: Type -> Type) :: [(SymNat, Type)] where
  -- Skip fields whose Haskell type is 'EmptyField' in the generic representation.
  GFieldsToHList ren sch tab (M1 S sel (K1 R EmptyField) :*: rest) =
    GFieldsToHList ren sch tab rest
  GFieldsToHList ren sch tab (M1 S sel (K1 R t) :*: rest) =
    AddOneField ren sch tab (GSelName sel) t (GFieldsToHList ren sch tab rest)
  GFieldsToHList ren sch tab U1 = '[]

-- | HListTag representation of Rep r (single constructor).
type family GHListTagRep ren sch tab (rep :: Type -> Type) :: [(SymNat, Type)] where
  GHListTagRep ren sch tab (D1 _d (C1 _c fields)) =
    GFieldsToHList ren sch tab fields

-- | HListTag representation: closed type family to avoid overlap with (:..) and PgTagged.
type family HListTagRep ren sch tab r :: [(SymNat, Type)] where
  HListTagRep ren sch tab (a :.. b) =
    Normalize (AppendHList (HListTagRep ren sch tab a) (HListTagRep ren sch tab b))
  HListTagRep ren sch tab (PgTagged (s :: Symbol) EmptyField) = '[]
  HListTagRep ren sch tab (PgTagged (s :: Symbol) t) =
    '[ '( '(Apply ren s, 0), FieldHListType ren sch tab s t) ]
  HListTagRep ren sch tab r = GHListTagRep ren sch tab (Rep r)

-- | Iso between record @r@ and HListTag for table @tab@ in schema @sch@.
class (Renamer ren) => IsoHListTag ren sch (tab :: NameNSK) r where
  toHListTag   :: r -> HListTag (HListTagRep ren sch tab r)
  fromHListTag :: HListTag (HListTagRep ren sch tab r) -> r

  default toHListTag
    :: ( Generic r
       , rep ~ Rep r
       , HListTagRep ren sch tab r ~ GHListTagRep ren sch tab rep
       , GIsoHListTag ren sch tab rep
       )
    => r -> HListTag (HListTagRep ren sch tab r)
  toHListTag = (gToHListTag @ren @sch @tab @(Rep r)) . from

  default fromHListTag
    :: ( Generic r
       , rep ~ Rep r
       , HListTagRep ren sch tab r ~ GHListTagRep ren sch tab rep
       , GIsoHListTag ren sch tab rep
       )
    => HListTag (HListTagRep ren sch tab r) -> r
  fromHListTag = to . (gFromHListTag @ren @sch @tab @(Rep r))

class GIsoHListTag ren sch tab (rep :: Type -> Type) where
  gToHListTag   :: rep x -> HListTag (GHListTagRep ren sch tab rep)
  gFromHListTag :: HListTag (GHListTagRep ren sch tab rep) -> rep x

instance (ts ~ GFieldsToHList ren sch tab fields, GFieldsIso ren sch tab fields ts)
  => GIsoHListTag ren sch tab (D1 d (C1 c fields)) where
  gToHListTag   (M1 (M1 x)) = gFieldsToHList @ren @sch @tab @fields @ts x
  gFromHListTag h           = M1 (M1 (gFieldsFromHList @ren @sch @tab @fields @ts h))

class GFieldsIso ren sch tab (fields :: Type -> Type) (ts :: [(SymNat, Type)]) where
  gFieldsToHList   :: fields x -> HListTag ts
  gFieldsFromHList :: HListTag ts -> fields x

instance GFieldsIso ren sch tab U1 '[] where
  gFieldsToHList   U1 = HNil
  gFieldsFromHList HNil = U1

-- Skip fields whose Haskell type is 'EmptyField' in the generic Iso: they do
-- not appear in 'HListTag', and on the way back we fill them with 'emptyField'.
instance
  ( GFieldsIso ren sch tab rest ts
  ) =>
  GFieldsIso ren sch tab (M1 S (MetaSel ('Just fld) u v w) (K1 R EmptyField) :*: rest) ts
  where
  gFieldsToHList (M1 (K1 _) :*: rest) =
    gFieldsToHList @ren @sch @tab @rest @ts rest
  gFieldsFromHList h =
    M1 (K1 emptyField) :*: gFieldsFromHList @ren @sch @tab @rest @ts h

instance {-# OVERLAPPING #-}
  ( GFieldsIso ren sch tab rest ts
  , CFieldInfo sch tab fld
  , sn ~ Apply ren fld
  , rf ~ TFieldInfo sch tab fld
  , rf ~ 'RFPlain _fd
  ) =>
  GFieldsIso ren sch tab (M1 S (MetaSel ('Just fld) u v w) (K1 R t) :*: rest)
    ( '( '(sn, n), t) ': ts)
  where
  gFieldsToHList (M1 (K1 v) :*: rest) = PgTag v :* gFieldsToHList @ren @sch @tab @rest @ts rest
  gFieldsFromHList (PgTag v :* rest) = M1 (K1 v) :*: gFieldsFromHList @ren @sch @tab @rest @ts rest

instance {-# OVERLAPPABLE #-}
  ( GFieldsIso ren sch tab rest ts
  , CFieldInfo sch tab fld
  , sn ~ Apply ren fld
  , ft ~ FieldHListType ren sch tab fld t
  , rf ~ TFieldInfo sch tab fld
  , rf ~ 'RFFromHere _toTab refs
  , RFFromHereNullable refs t
  , ToHListField ren sch tab fld t
  , FromHListField ren sch tab fld t
  ) =>
  GFieldsIso ren sch tab (M1 S (MetaSel ('Just fld) u v w) (K1 R t) :*: rest)
    ( '( '(sn, n), ft) ': ts)
  where
  gFieldsToHList (M1 (K1 v) :*: rest) =
    PgTag (toHListField @ren @sch @tab @fld @t v) :* gFieldsToHList @ren @sch @tab @rest @ts rest
  gFieldsFromHList (PgTag v :* rest) =
    M1 (K1 (fromHListField @ren @sch @tab @fld @t v)) :*: gFieldsFromHList @ren @sch @tab @rest @ts rest

instance {-# OVERLAPPING #-}
  ( GFieldsIso ren sch tab rest ts
  , CFieldInfo sch tab fld
  , sn ~ Apply ren fld
  , IsoHListTag ren sch fromTab child
  , htr ~ HListTagRep ren sch fromTab child
  , rf ~ TFieldInfo sch tab fld
  , rf ~ 'RFToHere fromTab _refs
  ) =>
  GFieldsIso ren sch tab (M1 S (MetaSel ('Just fld) u v w) (K1 R (SchList child)) :*: rest)
    ( '( '(sn, n), SchList (HListTag htr)) ': ts)
  where
  gFieldsToHList (M1 (K1 (SchList vs)) :*: rest) =
    PgTag (SchList (map (toHListTag @ren @sch @fromTab) vs))
      :* gFieldsToHList @ren @sch @tab @rest @ts rest
  gFieldsFromHList (PgTag (SchList hs) :* rest) =
    M1 (K1 (SchList (map (fromHListTag @ren @sch @fromTab) hs)))
      :*: gFieldsFromHList @ren @sch @tab @rest @ts rest

class ToHListField ren sch tab (fld :: Symbol) t where
  toHListField :: t -> FieldHListType ren sch tab fld t
class FromHListField ren sch tab (fld :: Symbol) t where
  fromHListField :: FieldHListType ren sch tab fld t -> t

class ToHListField' (rf :: RecFieldK NameNSK) ren sch tab (fld :: Symbol) t where
  toHListField' :: t -> FieldHListType ren sch tab fld t
class FromHListField' (rf :: RecFieldK NameNSK) ren sch tab (fld :: Symbol) t where
  fromHListField' :: FieldHListType ren sch tab fld t -> t

instance (rf ~ TFieldInfo sch tab fld, ToHListField' rf ren sch tab fld t)
  => ToHListField ren sch tab fld t where
  toHListField = toHListField' @rf @ren @sch @tab @fld @t
instance (rf ~ TFieldInfo sch tab fld, FromHListField' rf ren sch tab fld t)
  => FromHListField ren sch tab fld t where
  fromHListField = fromHListField' @rf @ren @sch @tab @fld @t

instance (FieldHListType ren sch tab fld t ~ t)
  => ToHListField' ('RFAggr fd fname canAny) ren sch tab fld t where
  toHListField' = id
instance (FieldHListType ren sch tab fld t ~ t)
  => FromHListField' ('RFAggr fd fname canAny) ren sch tab fld t where
  fromHListField' = id

instance (FieldHListType ren sch tab fld t ~ t)
  => ToHListField' ('RFPlain _fd) ren sch tab fld t where
  toHListField' = id
instance (FieldHListType ren sch tab fld t ~ t)
  => FromHListField' ('RFPlain _fd) ren sch tab fld t where
  fromHListField' = id

instance
  ( IsMaybe t ~ 'False
  , IsoHListTag ren sch toTab t
  , FieldHListType ren sch tab fld t ~ HListTag (HListTagRep ren sch toTab t)
  )
  => ToHListField' ('RFFromHere toTab _refs) ren sch tab fld t where
  toHListField' = toHListTag @ren @sch @toTab
instance
  ( IsMaybe t ~ 'False
  , IsoHListTag ren sch toTab t
  , FieldHListType ren sch tab fld t ~ HListTag (HListTagRep ren sch toTab t)
  )
  => FromHListField' ('RFFromHere toTab _refs) ren sch tab fld t where
  fromHListField' = fromHListTag @ren @sch @toTab

instance
  ( Generic t
  , IsoHListTag ren sch toTab t
  , FieldHListType ren sch tab fld (Maybe t) ~ Maybe (HListTag (HListTagRep ren sch toTab t))
  )
  => ToHListField' ('RFFromHere toTab _refs) ren sch tab fld (Maybe t) where
  toHListField' = fmap (toHListTag @ren @sch @toTab @t)
instance
  ( Generic t
  , IsoHListTag ren sch toTab t
  , FieldHListType ren sch tab fld (Maybe t) ~ Maybe (HListTag (HListTagRep ren sch toTab t))
  )
  => FromHListField' ('RFFromHere toTab _refs) ren sch tab fld (Maybe t) where
  fromHListField' = fmap (fromHListTag @ren @sch @toTab @t)

instance
  ( IsoHListTag ren sch fromTab child
  , FieldHListType ren sch tab fld (SchList child)
      ~ SchList (HListTag (HListTagRep ren sch fromTab child))
  )
  => ToHListField' ('RFToHere fromTab _refs) ren sch tab fld (SchList child) where
  toHListField' = SchList . map (toHListTag @ren @sch @fromTab) . getSchList
instance
  ( IsoHListTag ren sch fromTab child
  , FieldHListType ren sch tab fld (SchList child)
      ~ SchList (HListTag (HListTagRep ren sch fromTab child))
  )
  => FromHListField' ('RFToHere fromTab _refs) ren sch tab fld (SchList child) where
  fromHListField' = SchList . map (fromHListTag @ren @sch @fromTab) . getSchList

-- | Single tagged field: CFieldInfo + FieldHListType (same as Generic).
instance {-# OVERLAPPABLE #-}
  ( Renamer ren
  , CFieldInfo sch tab s
  , ToHListField ren sch tab s t
  , FromHListField ren sch tab s t
  , HListTagRep ren sch tab (PgTagged s t) ~ '[ '( '(Apply ren s, 0), FieldHListType ren sch tab s t) ]
  ) => IsoHListTag ren sch tab (PgTagged (s :: Symbol) t)
  where
  toHListTag (PgTag v) = PgTag (toHListField @ren @sch @tab @s @t v) :* HNil
  fromHListTag (PgTag v :* HNil) = PgTag (fromHListField @ren @sch @tab @s @t v)

-- | Tagged 'EmptyField': corresponds to an empty 'HListTag' ('HNil').
instance {-# OVERLAPPING #-}
  ( Renamer ren
  ) => IsoHListTag ren sch tab (PgTagged (s :: Symbol) EmptyField)
  where
  toHListTag _ = HNil
  fromHListTag HNil = PgTag emptyField

-- | Product: concatenate and normalize (renumber duplicate symbols 0, 1, 2, ...).
instance {-# OVERLAPPING #-}
  ( IsoHListTag ren sch tab a
  , IsoHListTag ren sch tab b
  , xs ~ HListTagRep ren sch tab a
  , ys ~ HListTagRep ren sch tab b
  , HListAppend xs ys
  , NormalizeHListTag (AppendHList xs ys)
  , SplitAtHListTagN (Length xs) (Normalize (AppendHList xs ys))
  , RetagHListTag (Take (Length xs) (Normalize (AppendHList xs ys))) xs
  , RetagHListTag (Drop (Length xs) (Normalize (AppendHList xs ys))) ys
  ) => IsoHListTag ren sch tab (a :.. b)
  where
  toHListTag (a :.. b) =
    normalizeHListTag (appendHListTag (toHListTag @ren @sch @tab a) (toHListTag @ren @sch @tab b))
  fromHListTag h =
    let (h1, h2) = splitAtHListTagN @(Length (HListTagRep ren sch tab a)) @(Normalize (AppendHList (HListTagRep ren sch tab a) (HListTagRep ren sch tab b))) h
        ha = retagHListTag @(Take (Length (HListTagRep ren sch tab a)) (Normalize (AppendHList (HListTagRep ren sch tab a) (HListTagRep ren sch tab b)))) @(HListTagRep ren sch tab a) h1
        hb = retagHListTag @(Drop (Length (HListTagRep ren sch tab a)) (Normalize (AppendHList (HListTagRep ren sch tab a) (HListTagRep ren sch tab b)))) @(HListTagRep ren sch tab b) h2
    in fromHListTag @ren @sch @tab ha :.. fromHListTag @ren @sch @tab hb

-- | Exclude (:..) so it uses the dedicated instance above.
type family IsPgProduct (r :: Type) :: Bool where
  IsPgProduct (a :.. b) = 'True
  IsPgProduct r = 'False

instance
  ( IsPgProduct r ~ 'False
  , Renamer ren
  , Generic r
  , rep ~ Rep r
  , GIsoHListTag ren sch tab rep
  , HListTagRep ren sch tab r ~ GHListTagRep ren sch tab (Rep r)
  )
  => IsoHListTag ren sch tab r
  where
  toHListTag = (gToHListTag @ren @sch @tab @(Rep r)) . from
  fromHListTag = to . (gFromHListTag @ren @sch @tab @(Rep r))
