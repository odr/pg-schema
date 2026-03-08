-- {-# OPTIONS_HADDOCK hide #-}
-- {-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE UndecidableInstances #-}
module PgSchema.PostgreSQL.HList.Class
  (IsoHList(..), Renamer(..), RenamerId)
  where

import Data.Coerce
import Data.Kind
import Data.Tagged
import PgSchema.PostgreSQL.Convert
import PgSchema.PostgreSQL.HList.Internal
import PgSchema.PostgreSQL.HList.Type
import PgSchema.PostgreSQL.HList.Utils
import Database.PostgreSQL.Simple
import PgSchema.Types.Aggr
import PgSchema.Schema.Def
import GHC.Generics
import GHC.TypeLits
import PgSchema.Tagged
import Prelude.Singletons qualified as SP


-- | Class for type-level string transformation "functions" using defunctionalization.
--
-- User have to create its own type and make instance of 'Renamer' using closed Type Family. E.g.
--
-- @
-- data MyRenamer
-- type family MyRenamerImpl s where
--   MyRenamerImpl "myField" = "my_field"
--   MyRenamerImpl s = s
-- instance Renamer MyRenamer where
--   type Apply MyRenamer s = MyRenamerImpl s
-- @
--
class Renamer r where
  type Apply r (s :: Symbol) :: Symbol

-- | Identity renamer: keeps field names as is
data RenamerId
instance Renamer RenamerId where
  type Apply RenamerId s = s

--------------------------------------------------------------------------------
-- IsoHList: Generic record <-> HList
--------------------------------------------------------------------------------

-- | Iso between record @r@ and HList for table @tab@ in schema @sch@.
--
-- There are instances for
--
-- - 'Generic' records
-- - Records composition with '(:.)'
-- - 'Tagged' values (':=' and '=:' are useful here)
--
-- Fields have to correspond to database names (field names or reference constraint name)
-- after renaming with 'Renamer'
--
-- I.e. any records with 'Generic' instances with appropriate fields are satisfied to 'IsoHList' automatically
--
class IsoHList (ren :: Type) (sch :: Type) (tab :: NameNSK) (r :: Type) where
  type HListRep ren sch tab r :: [(SymNat, Type)]
  toHList   :: r -> HList (HListRep ren sch tab r)
  fromHList :: HList (HListRep ren sch tab r) -> r

data IsoCase = TaggedCase | ProductCase | GenericCase

type family TFIsoCase t :: IsoCase where
  TFIsoCase (a :. b) = ProductCase
  TFIsoCase (a := b) = TaggedCase
  TFIsoCase t = GenericCase

class IsoHListIsoCase (ren :: Type) (sch :: Type) (tab :: NameNSK) t (ic :: IsoCase) where
  type HListRepIsoCase ren sch tab t ic :: [(SymNat, Type)]
  toHListIsoCase :: t -> HList (HListRepIsoCase ren sch tab t ic)
  fromHListIsoCase :: HList (HListRepIsoCase ren sch tab t ic) -> t

instance IsoHListIsoCase ren sch tab t (TFIsoCase t) => IsoHList ren sch tab t where
  type HListRep ren sch tab t = HListRepIsoCase ren sch tab t (TFIsoCase t)
  toHList = toHListIsoCase @ren @sch @tab @t @(TFIsoCase t)
  fromHList = fromHListIsoCase @ren @sch @tab @t @(TFIsoCase t)

instance (Generic r, rep ~ Rep r, GIsoHList ren sch tab rep)
  => IsoHListIsoCase ren sch tab r 'GenericCase where
  type HListRepIsoCase ren sch tab r 'GenericCase = GHListRep ren sch tab (Rep r)
  toHListIsoCase = gToHList @ren @sch @tab @(Rep r) . from
  fromHListIsoCase = to . (gFromHList @ren @sch @tab @(Rep r))


class GIsoHList (ren :: Type) (sch :: Type) (tab :: NameNSK) (rep :: Type -> Type) where
  type GHListRep ren sch tab rep :: [(SymNat, Type)]
  gToHList   :: rep x -> HList (GHListRep ren sch tab rep)
  gFromHList :: HList (GHListRep ren sch tab rep) -> rep x

instance (GIsoHList ren sch tab fields, NormalizeHList (GHListRep ren sch tab fields)) => GIsoHList ren sch tab (D1 d (C1 c fields)) where
  type GHListRep ren sch tab (D1 d (C1 c fields)) = Normalize (GHListRep ren sch tab fields)
  gToHList (M1 (M1 x)) = normalizeHList (gToHList @ren @sch @tab @fields x)
  gFromHList = M1 . M1 . gFromHList @ren @sch @tab @fields . denormalizeHList

instance (GIsoHList ren sch tab a, GIsoHList ren sch tab b
  , HListAppend (GHListRep ren sch tab a) (GHListRep ren sch tab b)
  , SplitAtHList (GHListRep ren sch tab a) (GHListRep ren sch tab b)
  )
  => GIsoHList ren sch tab (a :*: b)
  where
  type GHListRep ren sch tab (a :*: b) =
     GHListRep ren sch tab a SP.++ GHListRep ren sch tab b
  gToHList (a :*: b) =
    appendHList (gToHList @ren @sch @tab a) (gToHList @ren @sch @tab b)
  gFromHList ab =
    let (a, b) = splitAtHList @(GHListRep ren sch tab a) @(GHListRep ren sch tab b) ab
      in gFromHList @ren @sch @tab a :*: gFromHList @ren @sch @tab b

instance (CHListRepTypeCase ren sch tab fld t tc, tc ~ GetTypeCase t)
  => GIsoHList ren sch tab (S1 (MetaSel ('Just fld) u v w) (Rec0 t))
  where
  type GHListRep ren sch tab (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) =
    HListRepTypeCase ren sch tab fld t (GetTypeCase t)
  gToHList (M1 (K1 t)) = toHListTypeCase @ren @sch @tab @fld @t @(GetTypeCase t) t
  gFromHList h = M1 $ K1 $ fromHListTypeCase @ren @sch @tab @fld @t @(GetTypeCase t) h

data TypeCase
  = EmptyCase -- ^ drop field in HList
  | AggrCountCase -- ^ No FieldInfo
  | CommonCase

type family GetTypeCase t :: TypeCase where
  GetTypeCase () = 'EmptyCase
  GetTypeCase (Aggr "count" t) = 'AggrCountCase
  GetTypeCase (Aggr' "count" t) = 'AggrCountCase
  GetTypeCase t = 'CommonCase

class CHListRepTypeCase ren sch tab fld t typeCase where
  type HListRepTypeCase ren sch tab fld t typeCase :: [(SymNat, Type)]
  toHListTypeCase :: t -> HList (HListRepTypeCase ren sch tab fld t typeCase)
  fromHListTypeCase :: HList (HListRepTypeCase ren sch tab fld t typeCase) -> t

-- 1. Drop `()`
-- 2. Process (Aggr "count")
-- 3. Renamer
-- 4. FieldInfo
-- 4.1. RFPlain
-- 4.2. RFFromHere
-- 4.2.1 RFFromHere Maybe
-- 4.2.2 RFFromHere Non-Maybe
-- 4.3. RFToHere
instance CHListRepTypeCase ren sch tab fld () 'EmptyCase where
  type HListRepTypeCase ren sch tab fld () 'EmptyCase = '[]
  toHListTypeCase _ = HNil
  fromHListTypeCase _ = ()

instance (CHListRepRen ren sch tab s t, s ~ Apply ren fld)
  => CHListRepTypeCase ren sch tab fld t 'CommonCase where
    type HListRepTypeCase ren sch tab fld t 'CommonCase = HListRepRen ren sch tab (Apply ren fld) t
    toHListTypeCase = toHListRen @ren @sch @tab @s @t
    fromHListTypeCase = fromHListRen @ren @sch @tab @s @t

instance CHListRepTypeCase ren sch tab (fld :: Symbol) t 'AggrCountCase where
  type HListRepTypeCase ren sch tab fld t 'AggrCountCase = '[ '( '(fld, 0), t)]
  toHListTypeCase t = t :* HNil
  fromHListTypeCase (t :* HNil) = t

class CHListRepRen ren sch tab s t where
  type HListRepRen ren sch tab s t :: [(SymNat, Type)]
  toHListRen :: t -> HList (HListRepRen ren sch tab s t)
  fromHListRen :: HList (HListRepRen ren sch tab s t) -> t

instance (CDBFieldInfo sch tab fld, TDBFieldInfo sch tab fld ~ fi
  , CHListRepFi ren sch tab fld fi t) => CHListRepRen ren sch tab fld t
  where
    type HListRepRen ren sch tab fld t =
      GHListRepFi ren sch tab fld (TDBFieldInfo sch tab fld) t
    toHListRen = toHListFi @ren @sch @tab @fld @fi @t
    fromHListRen = fromHListFi @ren @sch @tab @fld @fi @t

class CHListRepFi ren sch tab (fld :: Symbol) fi t where
  type GHListRepFi ren sch tab fld fi t :: [(SymNat, Type)]
  toHListFi :: t -> HList (GHListRepFi ren sch tab fld fi t)
  fromHListFi :: HList (GHListRepFi ren sch tab fld fi t) -> t

instance (CanConvert sch tab fld t, Coercible (PlainType sch tab fld t fd) t)
  => CHListRepFi ren sch tab fld (RFPlain (fd :: FldDefK)) t
  where
    type GHListRepFi ren sch tab fld (RFPlain fd) t = '[ '( '(fld, 0), PlainType sch tab fld t fd)]
    toHListFi t = coerce t :* HNil
    fromHListFi (t :* HNil) = coerce t

type family PlainType sch tab fld t fd :: Type where
  PlainType sch tab fld (PgArr t) ('FldDef tn False def) = -- TypeError (Text "PgArr")
    Tagged (TypElem (TTypDef sch tn)) (PgArr t)
  PlainType sch tab fld (Maybe (PgArr t)) ('FldDef tn True def) = --TypeError (Text "PgArr")
    Maybe (Tagged (TypElem (TTypDef sch tn)) (PgArr t))
  PlainType sch tab fld t fd = t

instance (CHListRepFromMaybe ren sch fld toTab t b, b ~ IsMaybe t)
  => CHListRepFi ren sch tab fld (RFFromHere toTab refs) t where
  type GHListRepFi ren sch tab fld (RFFromHere toTab refs) t =
    HListRepFromMaybe ren sch fld toTab t (IsMaybe t)
  toHListFi = toHListFromMaybe @ren @sch @fld @toTab @t @b
  fromHListFi = fromHListFromMaybe @ren @sch @fld @toTab @t @b

class CHListRepFromMaybe ren sch (fld :: Symbol) toTab t b where
  type HListRepFromMaybe ren sch fld toTab t b :: [(SymNat, Type)]
  toHListFromMaybe :: t -> HList (HListRepFromMaybe ren sch fld toTab t b)
  fromHListFromMaybe :: HList (HListRepFromMaybe ren sch fld toTab t b) -> t

instance IsoHList ren sch toTab t
  => CHListRepFromMaybe ren sch fld toTab (Maybe t) 'True where
    type HListRepFromMaybe ren sch fld toTab (Maybe t) 'True =
      '[ '( '(fld, 0), Maybe (HList (HListRep ren sch toTab t))) ]
    toHListFromMaybe (Just t) = Just (toHList @ren @sch @toTab t) :* HNil
    toHListFromMaybe Nothing = Nothing :* HNil
    fromHListFromMaybe ((Just t) :* HNil) = Just $ fromHList @ren @sch @toTab t
    fromHListFromMaybe (Nothing :* HNil) = Nothing

instance IsoHList ren sch toTab t
  => CHListRepFromMaybe ren sch fld toTab t 'False where
    type HListRepFromMaybe ren sch fld toTab t 'False =
      '[ '( '(fld, 0), HList (HListRep ren sch toTab t)) ]
    toHListFromMaybe t = toHList @ren @sch @toTab t :* HNil
    fromHListFromMaybe (t :* HNil) = fromHList @ren @sch @toTab t

instance IsoHList ren sch fromTab t
  => CHListRepFi ren sch tab fld (RFToHere (fromTab :: NameNSK) refs) [t] where
  type GHListRepFi ren sch tab fld (RFToHere fromTab refs) [t] =
    '[ '( '(fld, 0), [HList (HListRep ren sch fromTab t)]) ]
  toHListFi xs = (toHList @ren @sch @fromTab <$> xs) :* HNil
  fromHListFi (xs :* HNil) = fromHList @ren @sch @fromTab <$> xs

--------------------------------------------------------------------------------
-- (:.) and PgTagged
--------------------------------------------------------------------------------
instance (IsoHList ren sch tab a, IsoHList ren sch tab b
  , HListAppend (HListRep ren sch tab a) (HListRep ren sch tab b)
  , SplitAtHList (HListRep ren sch tab a) (HListRep ren sch tab b)
  , NormalizeHList (HListRep ren sch tab a SP.++ HListRep ren sch tab b)
  )
  => IsoHListIsoCase ren sch tab (a :. b) 'ProductCase where
    type HListRepIsoCase ren sch tab (a :. b) 'ProductCase =
      Normalize (HListRep ren sch tab a SP.++ HListRep ren sch tab b)
    toHListIsoCase (a :. b) = normalizeHList
      $ appendHList (toHList @ren @sch @tab a) (toHList @ren @sch @tab b)
    fromHListIsoCase ab = fromHList @ren @sch @tab a :. fromHList @ren @sch @tab b
      where
        (a, b) = splitAtHList @(HListRep ren sch tab a) @(HListRep ren sch tab b)
          $ denormalizeHList ab

instance CHListRepTypeCase ren sch tab fld t (GetTypeCase t)
  => IsoHListIsoCase ren sch tab (fld := t) 'TaggedCase where
    type HListRepIsoCase ren sch tab (fld := t) 'TaggedCase =
      HListRepTypeCase ren sch tab fld t (GetTypeCase t)
    toHListIsoCase (Tagged t) =
      toHListTypeCase @ren @sch @tab @fld @t @(GetTypeCase t) t
    fromHListIsoCase = Tagged . fromHListTypeCase @ren @sch @tab @fld @t @(GetTypeCase t)
