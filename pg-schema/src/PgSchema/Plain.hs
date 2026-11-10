{-# LANGUAGE UndecidableInstances #-}
-- | Plain (untagged, flat) values of rows: 'rowsOf' / 'rowOf' and 'plains' / 'plain'.
--
-- A row is flattened the same way as for 'Database.PostgreSQL.Simple.ToRow' /
-- 'Database.PostgreSQL.Simple.FromRow' (see 'PgSchema.Ann.Cols'):
-- '(:.)' and Generic records are flattened into a sequence of fields,
-- @fld := ()@ gives no field. The structure (grouping) lives in the type only.
--
-- Here we have just structures: no schema is used.
module PgSchema.Plain
  ( Plain, PlainCols, PlainVal, PlainRow, rowsOf, plains, rowOf, plain ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits (Symbol)
import PgSchema.Ann (ColsCase(..), ColsCaseOf, NotProductTypeError)
import PgSchema.Types
import PgSchema.Utils.TF


-- | Flat (plain) value of a row @r@: values of all fields chained with '(:.)'
-- from the right, without field names. A row without fields is @()@.
--
-- Nested rows in field values (children @[r]@, parents @r@ or @Maybe r@) are
-- plain too.
--
-- >>> :kind! Plain ("a" := Int :. ("b" := Bool :. "c" := ()) :. "d" := ["e" := Char :. "f" := Char])
-- Plain ("a" := Int :. ("b" := Bool :. "c" := ()) :. "d" := ["e" := Char :. "f" := Char]) :: *
-- = Int :. (Bool :. [Char :. Char])
type Plain r = Chain (PlainCols r)

-- | Types of the fields of a row (like 'PgSchema.Ann.Cols' but schema-less).
type family PlainCols (r :: Type) :: [Type] where
  -- an optional group of fields cannot be split into fields:
  -- it is one value
  PlainCols (Maybe r) = '[Maybe (Plain r)]
  PlainCols (a :. b) = PlainCols a ++ PlainCols b
  PlainCols ((fld :: Symbol) := ()) = '[]
  PlainCols ((fld :: Symbol) := t) = '[PlainVal t]
  PlainCols r = GPlainCols (Rep r)

type family GPlainCols (rep :: Type -> Type) :: [Type] where
  GPlainCols (D1 d (C1 c flds)) = GPlainCols flds
  GPlainCols (a :*: b) = GPlainCols a ++ GPlainCols b
  GPlainCols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) = PlainCols (fld := t)
  GPlainCols rep = NotProductTypeError rep

-- | Plain value of a field.
--
-- Rows built with '(:.)' / '(:=)' (also in lists and 'Maybe') become plain.
-- Other values are not changed. In particular, Generic records in field values
-- are kept as is: without a schema they can't be distinguished from other
-- types having a Generic instance (e.g. 'Data.Aeson.Value').
type family PlainVal (t :: Type) :: Type where
  PlainVal [t] = [PlainVal t]
  PlainVal (Maybe t) = Maybe (PlainVal t)
  PlainVal (a :. b) = Plain (a :. b)
  PlainVal ((fld :: Symbol) := t) = Plain (fld := t)
  PlainVal t = t

type family Chain (ts :: [Type]) :: Type where
  Chain '[] = ()
  Chain '[t] = t
  Chain (t ': ts) = t :. Chain ts

--------------------------------------------------------------------------------
-- Value level
--------------------------------------------------------------------------------

data HL (ts :: [Type]) where
  HNil :: HL '[]
  (:&) :: t -> HL ts -> HL (t ': ts)
infixr 5 :&

appHL :: HL xs -> HL ys -> HL (xs ++ ys)
appHL HNil ys = ys
appHL (x :& xs) ys = x :& appHL xs ys

class ChainHL (ts :: [Type]) where
  toChain :: HL ts -> Chain ts
  fromChain :: Chain ts -> HL ts

instance ChainHL '[] where
  toChain _ = ()
  fromChain _ = HNil

instance ChainHL '[t] where
  toChain (x :& _) = x
  fromChain x = x :& HNil

instance (ChainHL (t2 ': ts), Chain (t1 ': t2 ': ts) ~ (t1 :. Chain (t2 ': ts)))
  => ChainHL (t1 ': t2 ': ts) where
  toChain (x :& xs) = x :. toChain xs
  fromChain (x :. xs) = x :& fromChain xs

data PlainCase = PCMaybe | PCRow ColsCase

type family PlainCaseOf (r :: Type) :: PlainCase where
  PlainCaseOf (Maybe r) = 'PCMaybe
  PlainCaseOf r = 'PCRow (ColsCaseOf r)

-- | Like 'PgSchema.Ann.ToRowCols': a row to the list of plain fields.
class ToPlainCols (c :: PlainCase) (cols :: [Type]) r where
  toPlainCols :: r -> HL cols

-- | Like 'PgSchema.Ann.FromRowCols': a row from the beginning of the list of
-- plain fields (as 'Database.PostgreSQL.Simple.FromRow.RowParser' takes the
-- beginning of the row). The rest is returned.
class FromPlainCols (c :: PlainCase) (cols :: [Type]) r (rest :: [Type]) where
  fromPlainCols :: HL (cols ++ rest) -> (r, HL rest)

-- (:=)
instance ToPlainCols ('PCRow 'NonGenericCase) '[] (fld := ()) where
  toPlainCols _ = HNil

instance FromPlainCols ('PCRow 'NonGenericCase) '[] (fld := ()) rest where
  fromPlainCols xs = (PgTag (), xs)

instance (CPlainVal (ValCaseOf t) t, v ~ PlainVal t)
  => ToPlainCols ('PCRow 'NonGenericCase) '[v] (fld := t) where
  toPlainCols (PgTag x) = toPlainVal @(ValCaseOf t) x :& HNil

instance (CPlainVal (ValCaseOf t) t, v ~ PlainVal t)
  => FromPlainCols ('PCRow 'NonGenericCase) '[v] (fld := t) rest where
  fromPlainCols (x :& xs) = (PgTag (fromPlainVal @(ValCaseOf t) x), xs)

-- (:.)
instance
  ( split ~ SplitAt (Length (PlainCols a)) cols
  , colsA ~ Fst split, colsB ~ Snd split, cols ~ (colsA ++ colsB)
  , ToPlainCols (PlainCaseOf a) colsA a, ToPlainCols (PlainCaseOf b) colsB b )
  => ToPlainCols ('PCRow 'NonGenericCase) cols (a :. b) where
  toPlainCols (a :. b) = appHL
    (toPlainCols @(PlainCaseOf a) @colsA a) (toPlainCols @(PlainCaseOf b) @colsB b)

instance
  ( split ~ SplitAt (Length (PlainCols a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , (cols ++ rest) ~ (colsA ++ (colsB ++ rest))
  , FromPlainCols (PlainCaseOf a) colsA a (colsB ++ rest)
  , FromPlainCols (PlainCaseOf b) colsB b rest )
  => FromPlainCols ('PCRow 'NonGenericCase) cols (a :. b) rest where
  fromPlainCols xs = (a :. b, r2)
    where
      (a, r1) = fromPlainCols @(PlainCaseOf a) @colsA @a @(colsB ++ rest) xs
      (b, r2) = fromPlainCols @(PlainCaseOf b) @colsB @b @rest r1

-- Maybe (optional group of fields)
instance (PlainRow r, v ~ Maybe (Plain r))
  => ToPlainCols 'PCMaybe '[v] (Maybe r) where
  toPlainCols x = fmap plain x :& HNil

instance (PlainRow r, v ~ Maybe (Plain r))
  => FromPlainCols 'PCMaybe '[v] (Maybe r) rest where
  fromPlainCols (x :& xs) = (fmap (rowOf r) x, xs)

-- Generic
instance (Generic r, GToPlainCols cols (Rep r))
  => ToPlainCols ('PCRow 'GenericCase) cols r where
  toPlainCols = gToPlainCols @cols . from

instance (Generic r, GFromPlainCols cols (Rep r) rest)
  => FromPlainCols ('PCRow 'GenericCase) cols r rest where
  fromPlainCols xs = let (x, r) = gFromPlainCols @cols @(Rep r) @rest xs in (to x, r)

class GToPlainCols (cols :: [Type]) (rep :: Type -> Type) where
  gToPlainCols :: rep x -> HL cols

class GFromPlainCols (cols :: [Type]) (rep :: Type -> Type) (rest :: [Type]) where
  gFromPlainCols :: HL (cols ++ rest) -> (rep x, HL rest)

instance GToPlainCols cols flds => GToPlainCols cols (D1 d (C1 c flds)) where
  gToPlainCols (M1 (M1 x)) = gToPlainCols @cols x

instance GFromPlainCols cols flds rest
  => GFromPlainCols cols (D1 d (C1 c flds)) rest where
  gFromPlainCols xs = let (x, r) = gFromPlainCols @cols @flds @rest xs in (M1 (M1 x), r)

instance
  ( split ~ SplitAt (Length (GPlainCols a)) cols
  , colsA ~ Fst split, colsB ~ Snd split, cols ~ (colsA ++ colsB)
  , GToPlainCols colsA a, GToPlainCols colsB b )
  => GToPlainCols cols (a :*: b) where
  gToPlainCols (a :*: b) = appHL (gToPlainCols @colsA a) (gToPlainCols @colsB b)

instance
  ( split ~ SplitAt (Length (GPlainCols a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , (cols ++ rest) ~ (colsA ++ (colsB ++ rest))
  , GFromPlainCols colsA a (colsB ++ rest), GFromPlainCols colsB b rest )
  => GFromPlainCols cols (a :*: b) rest where
  gFromPlainCols xs = (a :*: b, r2)
    where
      (a, r1) = gFromPlainCols @colsA @a @(colsB ++ rest) xs
      (b, r2) = gFromPlainCols @colsB @b @rest r1

instance ToPlainCols ('PCRow 'NonGenericCase) cols (fld := t)
  => GToPlainCols cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) where
  gToPlainCols (M1 (K1 x)) = toPlainCols @('PCRow 'NonGenericCase) (fld =: x)

instance FromPlainCols ('PCRow 'NonGenericCase) cols (fld := t) rest
  => GFromPlainCols cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) rest where
  gFromPlainCols xs = (M1 (K1 x), r)
    where
      (PgTag x, r) = fromPlainCols @('PCRow 'NonGenericCase) @cols @(fld := t) @rest xs

-- Field values
data ValCase = VCList | VCMaybe | VCRow | VCOther

type family ValCaseOf (t :: Type) :: ValCase where
  ValCaseOf [t] = 'VCList
  ValCaseOf (Maybe t) = 'VCMaybe
  ValCaseOf (a :. b) = 'VCRow
  ValCaseOf ((fld :: Symbol) := t) = 'VCRow
  ValCaseOf t = 'VCOther

class CPlainVal (c :: ValCase) t where
  toPlainVal :: t -> PlainVal t
  fromPlainVal :: PlainVal t -> t

instance (CPlainVal (ValCaseOf t) t, PlainVal [t] ~ [PlainVal t])
  => CPlainVal 'VCList [t] where
  toPlainVal = fmap (toPlainVal @(ValCaseOf t))
  fromPlainVal = fmap (fromPlainVal @(ValCaseOf t))

instance (CPlainVal (ValCaseOf t) t, PlainVal (Maybe t) ~ Maybe (PlainVal t))
  => CPlainVal 'VCMaybe (Maybe t) where
  toPlainVal = fmap (toPlainVal @(ValCaseOf t))
  fromPlainVal = fmap (fromPlainVal @(ValCaseOf t))

instance (PlainRow r, PlainVal r ~ Plain r) => CPlainVal 'VCRow r where
  toPlainVal = plain
  fromPlainVal = rowOf r

instance PlainVal t ~ t => CPlainVal 'VCOther t where
  toPlainVal = id
  fromPlainVal = id

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Row @r@ can be converted from and to its 'Plain' value.
class
  ( ToPlainCols (PlainCaseOf r) (PlainCols r) r
  , FromPlainCols (PlainCaseOf r) (PlainCols r) r '[]
  , ChainHL (PlainCols r), (PlainCols r ++ '[]) ~ PlainCols r )
  => PlainRow r

instance
  ( ToPlainCols (PlainCaseOf r) (PlainCols r) r
  , FromPlainCols (PlainCaseOf r) (PlainCols r) r '[]
  , ChainHL (PlainCols r), (PlainCols r ++ '[]) ~ PlainCols r )
  => PlainRow r

-- | Make a row of type @r@ from its plain value.
rowOf :: forall r -> PlainRow r => Plain r -> r
rowOf r = fst . fromPlainCols @(PlainCaseOf r) @(PlainCols r) @r @'[] . fromChain

-- | Plain value of a row. The inverse of 'rowOf'.
plain :: forall r. PlainRow r => r -> Plain r
plain = toChain . toPlainCols @(PlainCaseOf r) @(PlainCols r)

-- | Make rows of type @r@ from plain values: field names (and field types)
-- are taken from @r@, so values don't need names or type annotations.
--
-- > type Mid1 = "pos" := Int32 :. "flag" := Bool
-- > type Root = "code" := Text :. "grp" := Int32 :. "mid1RootFk" := [Mid1]
-- >
-- > insertJSON (MyAnn "root") conn $ rowsOf Root
-- >   [ "r1" :. 1 :. [10 :. True, 11 :. False]
-- >   , "r2" :. 2 :. [] ]
--
-- Rows are flattened as for 'Database.PostgreSQL.Simple.ToRow': parts joined
-- with '(:.)' and Generic records give their fields in order, so no
-- parentheses are needed whatever the grouping in the type is.
-- See 'Plain' for details.
--
-- A type written in place needs the @type@ keyword (@ExplicitNamespaces@),
-- otherwise '(:=)' is taken as the data constructor of comparison operators:
--
-- >>> rowsOf (type ("a" := Int :. ("b" := Bool :. "c" := ()))) [1 :. True, 2 :. False]
-- ["a" =: 1 :. ("b" =: True :. "c" =: ()),"a" =: 2 :. ("b" =: False :. "c" =: ())]
rowsOf :: forall r -> PlainRow r => [Plain r] -> [r]
rowsOf r = fmap (rowOf r)

-- | Plain values of rows (e.g. to pattern match on selected rows without
-- 'PgTag'). The inverse of 'rowsOf'.
--
-- >>> plains ["a" =: (1 :: Int) :. "b" =: True]
-- [1 :. True]
plains :: forall r. PlainRow r => [r] -> [Plain r]
plains = fmap plain
