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
import Database.Types.Aggr
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
       , HListTagRep ren sch tab r ~ Normalize (GHListTagRep ren sch tab rep)
       , GIsoHListTag ren sch tab rep
       , NormalizeHListTag (GHListTagRep ren sch tab rep)
       )
    => r -> HListTag (HListTagRep ren sch tab r)
  toHListTag = normalizeHListTag . gToHListTag @ren @sch @tab @(Rep r) . from

  default fromHListTag
    :: ( Generic r
       , rep ~ Rep r
       , HListTagRep ren sch tab r ~ GHListTagRep ren sch tab rep
       , GIsoHListTag ren sch tab rep
       )
    => HListTag (HListTagRep ren sch tab r) -> r
  fromHListTag = to . (gFromHListTag @ren @sch @tab @(Rep r))

class GIsoHListTag (ren :: Type) (sch :: Type) (tab :: NameNSK) (rep :: Type -> Type) where
  type GHListTagRep ren sch tab rep :: [(SymNat, Type)]
  gToHListTag   :: rep x -> HListTag (GHListTagRep ren sch tab rep)
  gFromHListTag :: HListTag (GHListTagRep ren sch tab rep) -> rep x

instance GIsoHListTag ren sch tab fields => GIsoHListTag ren sch tab (D1 d (C1 c fields)) where
  type GHListTagRep ren sch tab (D1 d (C1 c fields)) = GHListTagRep ren sch tab fields
  gToHListTag (M1 (M1 x)) = gToHListTag @ren @sch @tab @fields x
  gFromHListTag h = M1 (M1 (gFromHListTag @ren @sch @tab @fields h))

instance (GIsoHListTag ren sch tab a, GIsoHListTag ren sch tab b
  , HListAppend (GHListTagRep ren sch tab a) (GHListTagRep ren sch tab b)
  , SplitAtHListTag (GHListTagRep ren sch tab a) (GHListTagRep ren sch tab b)
  )
  => GIsoHListTag ren sch tab (a :*: b)
  where
  type GHListTagRep ren sch tab (a :*: b) =
    AppendHList (GHListTagRep ren sch tab a) (GHListTagRep ren sch tab b)
  gToHListTag (a :*: b) =
    appendHListTag (gToHListTag @ren @sch @tab a) (gToHListTag @ren @sch @tab b)
  gFromHListTag ab =
    let (a, b) = splitAtHListTag @(GHListTagRep ren sch tab a) @(GHListTagRep ren sch tab b) ab
      in gFromHListTag @ren @sch @tab a :*: gFromHListTag @ren @sch @tab b

instance (CGHListTagRepEmpty ren sch tab fld t b, b ~ IsEmptyField t)
  => GIsoHListTag ren sch tab (S1 (MetaSel ('Just fld) u v w) (Rec0 t))
  where
  type GHListTagRep ren sch tab (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) =
    GHListTagRepEmpty ren sch tab fld t (IsEmptyField t)
  gToHListTag (M1 (K1 t)) = gToHListTagEmpty @ren @sch @tab @fld @t @(IsEmptyField t) t
  gFromHListTag h = M1 $ K1 $ gFromHListTagEmpty @ren @sch @tab @fld @t @(IsEmptyField t) h

type family IsEmptyField t :: Bool where
  IsEmptyField EmptyField = 'True
  IsEmptyField _ = 'False

class CGHListTagRepEmpty ren sch tab fld t (b::Bool) where
  type GHListTagRepEmpty ren sch tab fld t b :: [(SymNat, Type)]
  gToHListTagEmpty :: t -> HListTag (GHListTagRepEmpty ren sch tab fld t b)
  gFromHListTagEmpty :: HListTag (GHListTagRepEmpty ren sch tab fld t b) -> t

instance CGHListTagRepEmpty ren sch tab fld EmptyField 'True where
  type GHListTagRepEmpty ren sch tab fld EmptyField 'True = '[]
  gToHListTagEmpty _ = HNil
  gFromHListTagEmpty _ = emptyField

instance (CGHListTagRepRen ren sch tab s t, s ~ Apply ren fld)
  => CGHListTagRepEmpty ren sch tab fld t 'False where
    type GHListTagRepEmpty ren sch tab fld t 'False = GHListTagRepRen ren sch tab (Apply ren fld) t
    gToHListTagEmpty = gToHListTagRen @ren @sch @tab @s @t
    gFromHListTagEmpty = gFromHListTagRen @ren @sch @tab @s @t

class CGHListTagRepRen ren sch tab s t where
  type GHListTagRepRen ren sch tab s t :: [(SymNat, Type)]
  gToHListTagRen :: t -> HListTag (GHListTagRepRen ren sch tab s t)
  gFromHListTagRen :: HListTag (GHListTagRepRen ren sch tab s t) -> t

instance (CFieldInfo sch tab fld, TFieldInfo sch tab fld ~ fi
  , CGHListTagRepFi ren sch tab fld fi t) => CGHListTagRepRen ren sch tab fld t
  where
    type GHListTagRepRen ren sch tab fld t =
      GHListTagRepFi ren sch tab fld (TFieldInfo sch tab fld) t
    gToHListTagRen = gToHListTagFi @ren @sch @tab @fld @fi @t
    gFromHListTagRen = gFromHListTagFi @ren @sch @tab @fld @fi @t

class CGHListTagRepFi (ren :: Type) (sch :: Type) (tab :: NameNSK) (fld :: Symbol) fi (t :: Type) where
  type GHListTagRepFi ren sch tab fld fi t :: [(SymNat, Type)]
  gToHListTagFi :: t -> HListTag (GHListTagRepFi ren sch tab fld fi t)
  gFromHListTagFi :: HListTag (GHListTagRepFi ren sch tab fld fi t) -> t

instance CGHListTagRepFi ren sch tab fld (RFPlain fd) t where
  type GHListTagRepFi ren sch tab fld (RFPlain fd) t = '[ '( '(fld, 0), t)]
  gToHListTagFi t = PgTag t :* HNil
  gFromHListTagFi (PgTag t :* HNil) = t

type family HListTagMaybe (ren :: Type) (sch :: Type) (tab :: NameNSK) (t :: Type) :: Type where
  HListTagMaybe ren sch tab (Maybe t) = Maybe (HListTag (HListTagRep ren sch tab t))
  HListTagMaybe ren sch tab t = HListTag (HListTagRep ren sch tab t)

instance (CGHListTagRepFromMaybe ren sch fld toTab t b, b ~ IsMaybe t)
  => CGHListTagRepFi ren sch tab fld (RFFromHere (toTab :: NameNSK) refs) t where
  type GHListTagRepFi ren sch tab fld (RFFromHere toTab refs) t = GHListTagRepFromMaybe ren sch fld toTab t (IsMaybe t)
  gToHListTagFi = gToHListTagFromMaybe @ren @sch @fld @toTab @t @b
  gFromHListTagFi = gFromHListTagFromMaybe @ren @sch @fld @toTab @t @b

class CGHListTagRepFromMaybe (ren :: Type) (sch :: Type) (fld :: Symbol) (toTab :: NameNSK) t (b::Bool) where
  type GHListTagRepFromMaybe ren sch fld toTab t b :: [(SymNat, Type)]
  gToHListTagFromMaybe :: t -> HListTag (GHListTagRepFromMaybe ren sch fld toTab t b)
  gFromHListTagFromMaybe :: HListTag (GHListTagRepFromMaybe ren sch fld toTab t b) -> t

instance (IsoHListTag ren sch toTab t) => CGHListTagRepFromMaybe ren sch fld toTab (Maybe t) 'True where
  type GHListTagRepFromMaybe ren sch fld toTab (Maybe t) 'True = '[ '( '(fld, 0), Maybe (HListTag (HListTagRep ren sch toTab t))) ]
  gToHListTagFromMaybe (Just t) = PgTag (Just $ toHListTag @ren @sch @toTab t) :* HNil
  gToHListTagFromMaybe Nothing = PgTag Nothing :* HNil
  gFromHListTagFromMaybe (PgTag (Just t) :* HNil) = Just $ fromHListTag @ren @sch @toTab t
  gFromHListTagFromMaybe (PgTag Nothing :* HNil) = Nothing

instance IsoHListTag ren sch toTab t => CGHListTagRepFromMaybe ren sch fld toTab t 'False where
  type GHListTagRepFromMaybe ren sch fld toTab t 'False = '[ '( '(fld, 0), HListTag (HListTagRep ren sch toTab t)) ]
  gToHListTagFromMaybe t = PgTag (toHListTag @ren @sch @toTab t) :* HNil
  gFromHListTagFromMaybe (PgTag t :* HNil) = fromHListTag @ren @sch @toTab t

instance IsoHListTag ren sch fromTab t
  => CGHListTagRepFi ren sch tab fld (RFToHere (fromTab :: NameNSK) refs) (SchList t) where
  type GHListTagRepFi ren sch tab fld (RFToHere fromTab refs) (SchList t) = '[ '( '(fld, 0), SchList (HListTag (HListTagRep ren sch fromTab t))) ]
  gToHListTagFi xs = PgTag (toHListTag @ren @sch @fromTab <$> xs) :* HNil
  gFromHListTagFi (PgTag xs :* HNil) = fromHListTag @ren @sch @fromTab <$> xs



-- | Constraint for RFFromHere: nullable refs must match Maybe type (variant B).
type family RFFromHereNullable refs t :: Constraint where
  RFFromHereNullable refs t =
    Assert (EqBool (HasNullableRefs refs) (IsMaybe t))
      (TypeError ( 'Text "IsoHListTag: RFFromHere field nullable must match type: "
                :<>: 'Text " HasNullableRefs = IsMaybe t"
                :$$: 'Text "References: " :<>: ShowType refs
                :$$: 'Text "User type:" :<>: ShowType t))

-- | HListTag field type from schema field kind and record field type.
type family FieldHListType' (ren :: Type) (sch :: Type) (rf :: RecFieldK NameNSK) (t :: Type) :: Type where
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
  HListTagRep ren sch tab r = Normalize (GHListTagRep ren sch tab (Rep r))

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
