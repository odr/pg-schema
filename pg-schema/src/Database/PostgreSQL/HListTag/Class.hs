{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.Class
  (IsoHListTag(..), Renamer(..), RenamerId)
  where

import Data.Kind
import Database.PostgreSQL.Simple
import Database.PostgreSQL.HListTag.Internal
import Database.PostgreSQL.HListTag.Type
import Database.PostgreSQL.HListTag.Utils
import Database.PostgreSQL.PgTagged
import Database.Types.Aggr
import Database.Types.EmptyField (EmptyField, emptyField)
import Database.Types.SchList (SchList (..))
import Database.Schema.Def
import GHC.Generics
import GHC.TypeLits
import Prelude.Singletons qualified as SP


-- | Class for type-level string transformation "functions"
class Renamer r where
  type Apply r (s :: Symbol) :: Symbol

-- | Identity renamer: keeps field names as is
data RenamerId
instance Renamer RenamerId where
  type Apply RenamerId s = s

--------------------------------------------------------------------------------
-- IsoHListTag: Generic record <-> HListTag
--------------------------------------------------------------------------------

-- | Iso between record @r@ and HListTag for table @tab@ in schema @sch@.
class IsoHListTag ren sch (tab :: NameNSK) r where
  type HListTagRep ren sch tab r :: [(SymNat, Type)]
  type HListTagRep ren sch tab r = GHListTagRep ren sch tab (Rep r)
  toHListTag   :: r -> HListTag (HListTagRep ren sch tab r)
  fromHListTag :: HListTag (HListTagRep ren sch tab r) -> r

  default toHListTag
    :: ( Generic r, rep ~ Rep r, GIsoHListTag ren sch tab rep
      , HListTagRep ren sch tab r ~ GHListTagRep ren sch tab rep )
    => r -> HListTag (HListTagRep ren sch tab r)
  toHListTag = gToHListTag @ren @sch @tab @(Rep r) . from

  default fromHListTag
    :: ( Generic r, rep ~ Rep r, GIsoHListTag ren sch tab rep
       , HListTagRep ren sch tab r ~ GHListTagRep ren sch tab rep )
    => HListTag (HListTagRep ren sch tab r) -> r
  fromHListTag = to . (gFromHListTag @ren @sch @tab @(Rep r))


class GIsoHListTag (ren :: Type) (sch :: Type) (tab :: NameNSK) (rep :: Type -> Type) where
  type GHListTagRep ren sch tab rep :: [(SymNat, Type)]
  gToHListTag   :: rep x -> HListTag (GHListTagRep ren sch tab rep)
  gFromHListTag :: HListTag (GHListTagRep ren sch tab rep) -> rep x

instance (GIsoHListTag ren sch tab fields, NormalizeHListTag (GHListTagRep ren sch tab fields)) => GIsoHListTag ren sch tab (D1 d (C1 c fields)) where
  type GHListTagRep ren sch tab (D1 d (C1 c fields)) = Normalize (GHListTagRep ren sch tab fields)
  gToHListTag (M1 (M1 x)) = normalizeHListTag (gToHListTag @ren @sch @tab @fields x)
  gFromHListTag = M1 . M1 . gFromHListTag @ren @sch @tab @fields . denormalizeHListTag

instance (GIsoHListTag ren sch tab a, GIsoHListTag ren sch tab b
  , HListAppend (GHListTagRep ren sch tab a) (GHListTagRep ren sch tab b)
  , SplitAtHListTag (GHListTagRep ren sch tab a) (GHListTagRep ren sch tab b)
  )
  => GIsoHListTag ren sch tab (a :*: b)
  where
  type GHListTagRep ren sch tab (a :*: b) =
     GHListTagRep ren sch tab a SP.++ GHListTagRep ren sch tab b
  gToHListTag (a :*: b) =
    appendHListTag (gToHListTag @ren @sch @tab a) (gToHListTag @ren @sch @tab b)
  gFromHListTag ab =
    let (a, b) = splitAtHListTag @(GHListTagRep ren sch tab a) @(GHListTagRep ren sch tab b) ab
      in gFromHListTag @ren @sch @tab a :*: gFromHListTag @ren @sch @tab b

instance (CHListTagRepTypeCase ren sch tab fld t tc, tc ~ GetTypeCase t)
  => GIsoHListTag ren sch tab (S1 (MetaSel ('Just fld) u v w) (Rec0 t))
  where
  type GHListTagRep ren sch tab (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) =
    HListTagRepTypeCase ren sch tab fld t (GetTypeCase t)
  gToHListTag (M1 (K1 t)) = toHListTagTypeCase @ren @sch @tab @fld @t @(GetTypeCase t) t
  gFromHListTag h = M1 $ K1 $ fromHListTagTypeCase @ren @sch @tab @fld @t @(GetTypeCase t) h

data TypeCase
  = EmptyCase -- ^ drop field in HList
  | AggrCountCase -- ^ No FieldInfo
  | CommonCase

type family GetTypeCase t :: TypeCase where
  GetTypeCase EmptyField = 'EmptyCase
  GetTypeCase (Aggr "count" t) = 'AggrCountCase
  GetTypeCase (Aggr' "count" t) = 'AggrCountCase
  GetTypeCase t = 'CommonCase

class CHListTagRepTypeCase ren sch tab fld t typeCase where
  type HListTagRepTypeCase ren sch tab fld t typeCase :: [(SymNat, Type)]
  toHListTagTypeCase :: t -> HListTag (HListTagRepTypeCase ren sch tab fld t typeCase)
  fromHListTagTypeCase :: HListTag (HListTagRepTypeCase ren sch tab fld t typeCase) -> t

-- 1. Drop EmptyField
-- 2. Process (Aggr "count")
-- 3. Renamer
-- 4. FieldInfo
-- 4.1. RFPlain
-- 4.2. RFFromHere
-- 4.2.1 RFFromHere Maybe
-- 4.2.2 RFFromHere Non-Maybe
-- 4.3. RFToHere
instance CHListTagRepTypeCase ren sch tab fld EmptyField 'EmptyCase where
  type HListTagRepTypeCase ren sch tab fld EmptyField 'EmptyCase = '[]
  toHListTagTypeCase _ = HNil
  fromHListTagTypeCase _ = emptyField

instance (CHListTagRepRen ren sch tab s t, s ~ Apply ren fld)
  => CHListTagRepTypeCase ren sch tab fld t 'CommonCase where
    type HListTagRepTypeCase ren sch tab fld t 'CommonCase = HListTagRepRen ren sch tab (Apply ren fld) t
    toHListTagTypeCase = toHListTagRen @ren @sch @tab @s @t
    fromHListTagTypeCase = fromHListTagRen @ren @sch @tab @s @t

instance CHListTagRepTypeCase ren sch tab (fld :: Symbol) t 'AggrCountCase where
  type HListTagRepTypeCase ren sch tab fld t 'AggrCountCase = '[ '( '(fld, 0), t)]
  toHListTagTypeCase t = PgTag t :* HNil
  fromHListTagTypeCase (PgTag t :* HNil) = t

class CHListTagRepRen ren sch tab s t where
  type HListTagRepRen ren sch tab s t :: [(SymNat, Type)]
  toHListTagRen :: t -> HListTag (HListTagRepRen ren sch tab s t)
  fromHListTagRen :: HListTag (HListTagRepRen ren sch tab s t) -> t

instance (CFieldInfo sch tab fld, TFieldInfo sch tab fld ~ fi
  , CHListTagRepFi ren sch tab fld fi t) => CHListTagRepRen ren sch tab fld t
  where
    type HListTagRepRen ren sch tab fld t =
      GHListTagRepFi ren sch tab fld (TFieldInfo sch tab fld) t
    toHListTagRen = toHListTagFi @ren @sch @tab @fld @fi @t
    fromHListTagRen = fromHListTagFi @ren @sch @tab @fld @fi @t

class CHListTagRepFi ren sch tab (fld :: Symbol) fi t where
  type GHListTagRepFi ren sch tab fld fi t :: [(SymNat, Type)]
  toHListTagFi :: t -> HListTag (GHListTagRepFi ren sch tab fld fi t)
  fromHListTagFi :: HListTag (GHListTagRepFi ren sch tab fld fi t) -> t

instance CHListTagRepFi ren sch tab fld (RFPlain fd) t where
  type GHListTagRepFi ren sch tab fld (RFPlain fd) t = '[ '( '(fld, 0), t)]
  toHListTagFi t = PgTag t :* HNil
  fromHListTagFi (PgTag t :* HNil) = t

instance (CHListTagRepFromMaybe ren sch fld toTab t b, b ~ IsMaybe t)
  => CHListTagRepFi ren sch tab fld (RFFromHere toTab refs) t where
  type GHListTagRepFi ren sch tab fld (RFFromHere toTab refs) t =
    HListTagRepFromMaybe ren sch fld toTab t (IsMaybe t)
  toHListTagFi = toHListTagFromMaybe @ren @sch @fld @toTab @t @b
  fromHListTagFi = fromHListTagFromMaybe @ren @sch @fld @toTab @t @b

class CHListTagRepFromMaybe ren sch (fld :: Symbol) toTab t b where
  type HListTagRepFromMaybe ren sch fld toTab t b :: [(SymNat, Type)]
  toHListTagFromMaybe :: t -> HListTag (HListTagRepFromMaybe ren sch fld toTab t b)
  fromHListTagFromMaybe :: HListTag (HListTagRepFromMaybe ren sch fld toTab t b) -> t

instance IsoHListTag ren sch toTab t
  => CHListTagRepFromMaybe ren sch fld toTab (Maybe t) 'True where
    type HListTagRepFromMaybe ren sch fld toTab (Maybe t) 'True =
      '[ '( '(fld, 0), Maybe (HListTag (HListTagRep ren sch toTab t))) ]
    toHListTagFromMaybe (Just t) = PgTag (Just $ toHListTag @ren @sch @toTab t) :* HNil
    toHListTagFromMaybe Nothing = PgTag Nothing :* HNil
    fromHListTagFromMaybe (PgTag (Just t) :* HNil) = Just $ fromHListTag @ren @sch @toTab t
    fromHListTagFromMaybe (PgTag Nothing :* HNil) = Nothing

instance IsoHListTag ren sch toTab t
  => CHListTagRepFromMaybe ren sch fld toTab t 'False where
    type HListTagRepFromMaybe ren sch fld toTab t 'False =
      '[ '( '(fld, 0), HListTag (HListTagRep ren sch toTab t)) ]
    toHListTagFromMaybe t = PgTag (toHListTag @ren @sch @toTab t) :* HNil
    fromHListTagFromMaybe (PgTag t :* HNil) = fromHListTag @ren @sch @toTab t

instance IsoHListTag ren sch fromTab t
  => CHListTagRepFi ren sch tab fld (RFToHere (fromTab :: NameNSK) refs) (SchList t) where
  type GHListTagRepFi ren sch tab fld (RFToHere fromTab refs) (SchList t) = '[ '( '(fld, 0), SchList (HListTag (HListTagRep ren sch fromTab t))) ]
  toHListTagFi xs = PgTag (toHListTag @ren @sch @fromTab <$> xs) :* HNil
  fromHListTagFi (PgTag xs :* HNil) = fromHListTag @ren @sch @fromTab <$> xs

--------------------------------------------------------------------------------
-- (:.) and PgTagged
--------------------------------------------------------------------------------
instance (IsoHListTag ren sch tab a, IsoHListTag ren sch tab b
  , HListAppend (HListTagRep ren sch tab a) (HListTagRep ren sch tab b)
  , SplitAtHListTag (HListTagRep ren sch tab a) (HListTagRep ren sch tab b)
  , NormalizeHListTag (HListTagRep ren sch tab a SP.++ HListTagRep ren sch tab b)
  )
  => IsoHListTag ren sch tab (a :. b) where
    type HListTagRep ren sch tab (a :. b) =
      Normalize (HListTagRep ren sch tab a SP.++ HListTagRep ren sch tab b)
    toHListTag (a :. b) = normalizeHListTag
      $ appendHListTag (toHListTag @ren @sch @tab a) (toHListTag @ren @sch @tab b)
    fromHListTag ab = fromHListTag @ren @sch @tab a :. fromHListTag @ren @sch @tab b
      where
        (a, b) = splitAtHListTag @(HListTagRep ren sch tab a) @(HListTagRep ren sch tab b)
          $ denormalizeHListTag ab

instance CHListTagRepTypeCase ren sch tab fld t (GetTypeCase t)
  => IsoHListTag ren sch tab (PgTagged fld t) where
    type HListTagRep ren sch tab (PgTagged fld t) =
      HListTagRepTypeCase ren sch tab fld t (GetTypeCase t)
    toHListTag (PgTag t) =
      toHListTagTypeCase @ren @sch @tab @fld @t @(GetTypeCase t) t
    fromHListTag = PgTag . fromHListTagTypeCase @ren @sch @tab @fld @t @(GetTypeCase t)
