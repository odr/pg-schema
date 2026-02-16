{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.PostgreSQL.HListTag
  ( HListTag(..)
  , IsoHListTag(..)
  , Renamer(..)
  , RenamerId
  ) where

import GHC.Generics
import GHC.TypeLits
import Data.Kind
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy
import Database.PostgreSQL.PgProduct
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Simple.FromField as PG hiding (Text)
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.ToRow as PG
import Database.Types.SchList

--------------------------------------------------------------------------------
-- 1. DATA STRUCTURE
--------------------------------------------------------------------------------

infixr 3 :*

-- | Heterogeneous list with Symbol tags (field names)
data HListTag (ts :: [(Symbol, Type)]) where
  HNil :: HListTag '[]
  (:*) :: PgTagged s t -> HListTag ts -> HListTag ('(s, t) ': ts)

instance Show (HListTag '[]) where
  show HNil = "HNil"

instance (KnownSymbol s, Show t, Show (HListTag ts)) => Show (HListTag ('(s, t) ': ts)) where
  show (x :* xs) = show x ++ " :* " ++ show xs

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
class HLift (ts :: [(Symbol, Type)]) where
  hlift :: (forall x. x -> f x)
        -> HListTag ts
        -> HListTag (Lifted ts f)

type family Lifted (ts :: [(Symbol, Type)]) (f :: Type -> Type) :: [(Symbol, Type)] where
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

instance (KnownSymbol s, ToJSON t, FromJSON t, HListToJSON ts)
      => HListToJSON ('(s, t) ': ts) where

  toSeriesFields (PgTagged val :* rest) =
    Key.fromString (symbolVal (Proxy @s)) .= val <> toSeriesFields rest

  toMapFields (PgTagged val :* rest) =
    KM.insert (Key.fromString $ symbolVal (Proxy @s)) (toJSON val) (toMapFields rest)

  parseFields km = do
    let keyString = symbolVal (Proxy @s)
        key = Key.fromString keyString

    -- Ищем значение в объекте
    case KM.lookup key km of
      Nothing -> fail $ "HListTag: missing key " ++ show keyString
      Just v  -> do
        -- Парсим значение и, если падает, добавляем контекст с именем поля
        val  <- parseJSON v <?> Key key
        rest <- parseFields km
        pure (PgTagged val :* rest)

-- Финальные инстансы
instance HListToJSON ts => ToJSON (HListTag ts) where
  toEncoding hlist = pairs (toSeriesFields hlist)
  toJSON     hlist = Object (toMapFields hlist)

instance HListToJSON ts => FromJSON (HListTag ts) where
  parseJSON = withObject "HListTag" $ \obj -> parseFields obj

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
-- 4. THE MAIN INTERFACE
--------------------------------------------------------------------------------

-- | Isomorphism between a record and a tagged HList.
--
-- __ Examples __
--
-- >>> data T = T { f1 :: String, f2 :: ("a" := [Int]) } deriving (Show, Generic)
-- >>> instance IsoHListTag RenamerId T
-- >>> toHListTag @RenamerId $ T "test" [1..3]
-- >>> fromHListTag @RenamerId (toHListTag @RenamerId $ T "test" [1..3]) :: T
-- "f1" =: ("test") :* "f2" =: ([1,2,3]) :* HNil
-- T {f1 = "test", f2 = [1,2,3]}

class IsoHListTag r a where
  type Fields r a :: [(Symbol, Type)]
  type Fields r a = RecordFields (Rep a) r

  toHListTag   :: a -> HListTag (Fields r a)
  fromHListTag :: HListTag (Fields r a) -> a

  default toHListTag
    :: ( Generic a
       , GToHListTag (Rep a) r
       , Fields r a ~ RecordFields (Rep a) r
       )
    => a -> HListTag (Fields r a)
  toHListTag = gToHListTag @(Rep a) @r . from

  default fromHListTag
    :: ( Generic a
       , Fields r a ~ RecordFields (Rep a) r
       , GFromHListTag (Rep a) r (Fields r a)
       )
    => HListTag (Fields r a) -> a
  fromHListTag h = let (x, _) = gFromHListTag @(Rep a) @r h in to x

instance IsoHListTag r (s := v) where
  type Fields r (s := v) = '[ '(s,v)]
  toHListTag  v = v :* HNil
  fromHListTag (v :* HNil) = v

instance ( IsoHListTag r a
         , IsoHListTag r b
         , AppendHListTag (Fields r a) (Fields r b)
         , SplitHListTag (Fields r a) (Fields r b)
         ) => IsoHListTag r (a :.. b) where

  type Fields r (a :.. b) = Append (Fields r a) (Fields r b)

  toHListTag (a :.. b) =
    appendHListTag (toHListTag @r a) (toHListTag @r b)

  fromHListTag h =
    let (h1, h2) = splitHListTag @(Fields r a) @(Fields r b) h
    in fromHListTag @r h1 :.. fromHListTag @r h2

-- >>> toHListTag @RenamerId $ ("f1" =: (5::Int)) :.. ("f2" =: True) :.. ("f3" =: [(1::Int)..3])
-- "f1" =: (5) :* "f2" =: (True) :* "f3" =: ([1,2,3]) :* HNil
-- >>> fromHListTag @RenamerId (toHListTag @RenamerId (("f1" =: (5::Int)) :.. ("f2" =: True) :.. ("f3" =: [(1::Int)..3]))) :: "f1" := Int :.. "f2" := Bool :.. "f3" := [Int]
-- "f1" =: (5) :.. ("f2" =: (True) :.. "f3" =: ([1,2,3]))

--------------------------------------------------------------------------------
-- 6. RECURSION (INTERNAL)
--------------------------------------------------------------------------------

-- | Типы действий при распаковке
data UnpackAction = DoUnpackRec | DoUnpackList | DoNothing

-- | Рекурсивный определитель действия
type family DecideAction t :: UnpackAction where
  DecideAction (HListTag ts)           = 'DoUnpackRec
  DecideAction (SchList (HListTag ts)) = 'DoUnpackList
  DecideAction (Maybe t)               = DecideAction t  -- Рекурсивно смотрим внутрь Maybe
  DecideAction _                       = 'DoNothing      -- Plain поля, EmptyField, и т.д.

class UnpackTagAction (action :: UnpackAction) r t_in t_out where
  unpackTagAction :: t_in -> t_out

-- Ветка 1: Plain (id)
instance (t_in ~ t_out) => UnpackTagAction 'DoNothing r t_in t_out where
  unpackTagAction = id

-- Ветка 2: Ныряем в рекорд (HListTag -> Record)
instance (IsoHListTag r a, Fields r a ~ ts)
    => UnpackTagAction 'DoUnpackRec r (HListTag ts) a where
  unpackTagAction = fromHListTag @r

-- Ветка 2.1: Ныряем в рекорд внутри Maybe (Maybe HListTag -> Maybe Record)
instance (IsoHListTag r a, Fields r a ~ ts)
    => UnpackTagAction 'DoUnpackRec r (Maybe (HListTag ts)) (Maybe a) where
  unpackTagAction = fmap (fromHListTag @r)

-- Ветка 3: Ныряем в список (SchList HListTag -> SchList Record)
instance (IsoHListTag r a, Fields r a ~ ts)
    => UnpackTagAction 'DoUnpackList r (SchList (HListTag ts)) (SchList a) where
  unpackTagAction (SchList xs) = SchList (map (fromHListTag @r) xs)

-- Ветка 3.1: Ныряем в список внутри Maybe (на всякий случай)
instance (IsoHListTag r a, Fields r a ~ ts)
    => UnpackTagAction 'DoUnpackList r (Maybe (SchList (HListTag ts))) (Maybe (SchList a)) where
  unpackTagAction = fmap (\(SchList xs) -> SchList (map (fromHListTag @r) xs))

class UnpackTag r t_in t_out where
  unpackTag :: t_in -> t_out

instance (action ~ DecideAction t_out, UnpackTagAction action r t_in t_out)
    => UnpackTag r t_in t_out where
  unpackTag = unpackTagAction @action @r

--------------------------------------------------------------------------------
-- 7. GENERIC ENGINE (Internal)
--------------------------------------------------------------------------------

type family RecordFields (f :: Type -> Type) (r :: Type) :: [(Symbol, Type)] where
  RecordFields (D1 m c) r = RecordFields c r
  RecordFields (C1 m f) r = RecordFields f r
  RecordFields (l :*: rest) r = Append (RecordFields l r) (RecordFields rest r)
  RecordFields (S1 ('MetaSel ('Just s) _ _ _) (K1 i t)) r = '[ '(Apply r s, t) ]
  RecordFields U1 _ = '[]

-- | To-conversion: straightforward recursion
class GToHListTag (f :: Type -> Type) (r :: Type) where
  gToHListTag :: f p -> HListTag (RecordFields f r)

instance GToHListTag c r => GToHListTag (D1 m c) r where
  gToHListTag (M1 x) = gToHListTag @c @r x

instance GToHListTag f r => GToHListTag (C1 m f) r where
  gToHListTag (M1 x) = gToHListTag @f @r x

instance Renamer r
    => GToHListTag (S1 ('MetaSel ('Just s) i c d) (K1 i2 t)) r where
    -- => GToHListTag (S1 m (K1 i t)) r where
  gToHListTag (M1 (K1 x)) = PgTag @(Apply r s) x :* HNil

instance (GToHListTag l r, GToHListTag rest r, AppendHListTag (RecordFields l r) (RecordFields rest r))
    => GToHListTag (l :*: rest) r where
  gToHListTag (l :*: rest) = appendHListTag (gToHListTag @l @r l) (gToHListTag @rest @r rest)

-- | From-conversion: structural mapping (one-to-one)
class GFromHListTag (f :: Type -> Type) (r :: Type) (ts :: [(Symbol, Type)]) where
  -- Возвращаем остаток списка (tail), который не был поглощен текущим конструктором
  gFromHListTag :: HListTag ts -> (f p, HListTag (Remaining ts f))

-- Вспомогательная TF для вычисления остатка списка после разбора части Generic
type family Remaining (ts :: [(Symbol, Type)]) (f :: Type -> Type) :: [(Symbol, Type)] where
  Remaining ts (D1 m c) = Remaining ts c
  Remaining ts (C1 m f) = Remaining ts f
  Remaining (x ': xs) (S1 m (K1 i t)) = xs
  Remaining ts (l :*: r) = Remaining (Remaining ts l) r
  Remaining ts U1 = ts

-- Реализация инстансов
instance GFromHListTag c r ts => GFromHListTag (D1 m c) r ts where
  gFromHListTag h = let (x, rest) = gFromHListTag @c @r h in (M1 x, rest)

instance GFromHListTag f r ts => GFromHListTag (C1 m f) r ts where
  gFromHListTag h = let (x, rest) = gFromHListTag @f @r h in (M1 x, rest)

instance (l_ts ~ Remaining ts l, GFromHListTag l r ts, GFromHListTag rest r l_ts)
    => GFromHListTag (l :*: rest) r ts where
  gFromHListTag h =
    let (left, h_mid) = gFromHListTag @l @r h
        (right, h_end) = gFromHListTag @rest @r h_mid
    in (left :*: right, h_end)

-- Вот здесь происходит вызов твоего UnpackTag
instance ( ts ~ ('(s_in, t_in) ': xs)
         , UnpackTag r t_in t_out -- Твой диспетчер из UnpackAction
         ) => GFromHListTag (S1 m (K1 i t_out)) r ts where
  gFromHListTag (PgTag x :* xs) = (M1 (K1 (unpackTag @r x)), xs)
--------------------------------------------------------------------------------
-- 8. TYPE-LEVEL UTILITIES
--------------------------------------------------------------------------------

-- | Concatenates lists. It has to be in `base` but it is not.
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family NoMember (s :: Symbol) (ts :: [(Symbol, Type)]) :: Constraint where
  NoMember s '[] = ()
  NoMember s ('(s, t) ': ts) = TypeError ('Text "Duplicate field: " ':<>: 'Text s)
  NoMember s (_ ': ts) = NoMember s ts

type family FindIndex (s :: Symbol) (ts :: [(Symbol, Type)]) :: Nat where
  FindIndex s ('(s, t) ': ts) = 0
  FindIndex s (f ': ts)       = 1 + FindIndex s ts

type family AtImpl (n :: Nat) (ts :: [(Symbol, Type)]) :: Type where
  AtImpl 0 ('(s, t) ': ts) = t
  AtImpl n (f ': ts)       = AtImpl (n - 1) ts

class GetAt (n :: Nat) (ts :: [(Symbol, Type)]) where
  getAt :: Proxy n -> HListTag ts -> AtImpl n ts

instance {-# OVERLAPPING #-} GetAt 0 ('(s, t) ': ts) where
  getAt _ (PgTag x :* _) = x

instance (GetAt (n - 1) ts, AtImpl n (f ': ts) ~ AtImpl (n - 1) ts) => GetAt n (f ': ts) where
  getAt _ (_ :* xs) = getAt (Proxy @(n - 1)) xs

-- | Concatenate two 'HListTag'
class AppendHListTag as bs where
  appendHListTag :: HListTag as -> HListTag bs -> HListTag (Append as bs)

instance AppendHListTag '[] bs where
  appendHListTag HNil ys = ys

instance AppendHListTag as bs => AppendHListTag (a ': as) bs where
  appendHListTag (x :* xs) ys = x :* appendHListTag xs ys

class SplitHListTag (as :: [(Symbol, Type)]) (bs :: [(Symbol, Type)]) where
  splitHListTag :: HListTag (Append as bs) -> (HListTag as, HListTag bs)

instance SplitHListTag '[] bs where
  splitHListTag h = (HNil, h)

instance SplitHListTag as bs => SplitHListTag (a ': as) bs where
  splitHListTag (x :* xs) =
    let (left, right) = splitHListTag @as @bs xs
    in (x :* left, right)
