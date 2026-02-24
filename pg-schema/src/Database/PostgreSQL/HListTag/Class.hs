{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.Class where

import Data.Kind
import Data.Type.Bool
import Database.PostgreSQL.HListTag.Internal
import Database.PostgreSQL.HListTag.Type
import Database.PostgreSQL.HListTag.Utils
import Database.PostgreSQL.PgProduct
import Database.PostgreSQL.PgTagged
import Database.Types.EmptyField (EmptyField, emptyField)
import Database.Types.SchList (SchList (..))
import Database.Schema.Def
import GHC.Generics
import GHC.TypeError
import GHC.TypeLits


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
class Renamer ren => IsoHListTag ren sch (tab :: NameNSK) r where
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

-- | HListTag representation: closed type family to avoid overlap with (:..) and PgTagged.
type family HListTagRep ren sch tab r :: [(SymNat, Type)] where
  HListTagRep ren sch tab (a :.. b) =
    Normalize (AppendHList (HListTagRep ren sch tab a) (HListTagRep ren sch tab b))
  HListTagRep ren sch tab (PgTagged (s :: Symbol) EmptyField) = '[]
  HListTagRep ren sch tab (PgTagged (s :: Symbol) t) =
    '[ '( '(Apply ren s, 0), FieldHListType ren sch tab s t) ]
  HListTagRep ren sch tab r = GHListTagRep ren sch tab (Rep r)

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
