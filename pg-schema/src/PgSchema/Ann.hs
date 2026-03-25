{-# LANGUAGE UndecidableInstances #-}
module PgSchema.Ann where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Data.Coerce
import Data.Singletons.TH (genDefunSymbols)
import Data.Type.Bool
import Data.Typeable
import Data.Text qualified as T
import Data.Kind
import Database.PostgreSQL.Simple.ToField(ToField(..), Action, toJSONField)
import Database.PostgreSQL.Simple.FromField(FromField(..), fromJSONField)
import Database.PostgreSQL.Simple.ToRow(ToRow(..))
import Database.PostgreSQL.Simple.FromRow(FromRow(..), RowParser, field)
import GHC.Generics
import GHC.Int
import GHC.TypeLits
import GHC.TypeError as TE
import PgSchema.Schema
import PgSchema.Types
import PgSchema.Utils.Internal
import Data.Singletons as SP
import PgSchema.Utils.TF as SP



-- $setup
-- >>> import PgSchema.Schema.Catalog
-- >>> import Database.PostgreSQL.Simple
-- >>> conn <- connectPostgreSQL ""

-- | Type-level annotation: enforce constraints at compile time
-- and drive demoted types used to generate correct SQL.
-- 'annRen' and 'annSch' are fixed for the whole DML-operation.
-- 'annDepth' and 'annTab' are changed while traversing the structure of the ADT.
--
data Ann = Ann
  { annRen  :: Renamer -- ^ Renamer to convert Haskell names to database names.
  , annSch  :: Type    -- ^ Schema with tables, relations and types.
  , annDepth :: Nat
  -- ^ Depth of the nested relations. It is mostly used to prevent cycles in types.
  , annTab  :: NameNSK -- ^ Name of the root table.
  }

type family AnnSch (ann :: Ann) where
  AnnSch ('Ann ren sch depth tab) = sch

data ColInfo (p :: Type) = ColInfo
  { ciField   :: SymNat
  , ciType    :: Type
  , ciDbField :: Symbol
  , ciInfo    :: RecField' Symbol p
  }

-- | Renamer is a type-level function from 'Symbol' to 'Symbol'.
type Renamer = Symbol ~> Symbol

-- | Apply renamer to symbol.
--
--  Like 'Data.Singletons.Apply' but specialized for 'Symbol'.
--
-- To make your own Renamer, typically you make
--
-- * data MyRenamer :: Renamer
-- * closed type family: `type family MyRenamerImpl (s :: Symbol) :: Symbol where ... `
-- * type instance ApplyRenamer MyRenamer s = MyRenamerImpl s
--
type family ApplyRenamer (renamer :: Renamer) (s :: Symbol) :: Symbol

-- | Renamer that does not change the symbol.
data RenamerId :: Renamer

type instance ApplyRenamer RenamerId s = s

--------------------------------------------------------------------------------
-- Case dispatch
--------------------------------------------------------------------------------

data ColsCase = NonGenericCase | GenericCase

type family ColsCaseOf (r :: Type) :: ColsCase where
  ColsCaseOf (a :. b)              = 'NonGenericCase
  ColsCaseOf ((_ :: Symbol) := _) = 'NonGenericCase
  ColsCaseOf r = 'GenericCase

--------------------------------------------------------------------------------
-- CCols / CColsCase
--------------------------------------------------------------------------------

class CColsCase ann r (ColsCaseOf r) => CCols ann r where
  type Cols ann r :: [ColInfo NameNSK]

instance CColsCase ann r (ColsCaseOf r) => CCols ann r where
  type Cols ann r = ColsWithCase ann r (ColsCaseOf r)

class CColsCase (ann :: Ann) (r :: Type) (c :: ColsCase) where
  type ColsWithCase ann r c :: [ColInfo NameNSK]

instance CColsCase ann r 'NonGenericCase where
  type ColsWithCase ann r 'NonGenericCase = ColsNonGeneric ann r

instance Generic r => CColsCase ann r 'GenericCase where
  type ColsWithCase ann r 'GenericCase = GCols ann (Rep r)

--------------------------------------------------------------------------------
-- ColsNonGeneric (closed TF: (:.) and (:=))
--------------------------------------------------------------------------------

type family ColsNonGeneric (ann :: Ann) r :: [ColInfo NameNSK] where
  ColsNonGeneric ann (a :. b) = Normalize (Cols ann a SP.++ Cols ann b)
  ColsNonGeneric ann (fld := t) = Col ann fld t

--------------------------------------------------------------------------------
-- Col / ColFI (per-field, reused by GCols)
--------------------------------------------------------------------------------

type family Col (ann :: Ann) (fld :: Symbol) t :: [ColInfo NameNSK] where
  Col ann fld () = '[]
  Col ann fld (Aggr ACount Int64) =
    '[ 'ColInfo '(fld, 0) (Aggr ACount Int64) fld
      ('RFAggr ('FldDef ("pg_catalog" ->> "int8") False False) 'ACount 'True) ]
  Col ann fld (Aggr' ACount Int64) =
    '[ 'ColInfo '(fld, 0) (Aggr' ACount Int64) fld
      ('RFAggr ('FldDef ("pg_catalog" ->> "int8") False False) 'ACount 'True) ]
  Col ('Ann ren sch d tab) fld t =
    ColFI ('Ann ren sch d tab) fld (TDBFieldInfo sch tab (ApplyRenamer ren fld)) t

type family ColFI (ann :: Ann) (fld :: Symbol) (fi :: RecFieldK NameNSK) t
  :: [ColInfo NameNSK] where
    ColFI ('Ann ren sch _ _) fld ('RFPlain ('FldDef tn False def)) (PgArr t) =
      '[ 'ColInfo '(fld, 0) (PgTag (TypElem (TTypDef sch tn)) (PgArr t))
        (ApplyRenamer ren fld) (RFPlain ('FldDef tn False def))]
    ColFI ('Ann ren sch _ _) fld ('RFPlain ('FldDef tn True def)) (Maybe (PgArr t)) =
      '[ 'ColInfo '(fld, 0) (Maybe (PgTag (TypElem (TTypDef sch tn)) (PgArr t)))
        (ApplyRenamer ren fld) (RFPlain ('FldDef tn True def))]
    -- RFFromHere: Maybe r
    ColFI ('Ann ren sch d _) fld ('RFFromHere (toTab :: NameNSK) refs) (Maybe r) =
      '[ 'ColInfo '(fld, 0) (Maybe (PgTag (AnnRefTabDepth ('Ann ren sch d toTab) toTab) r))
        (ApplyRenamer ren fld) ('RFFromHere toTab refs) ]
    -- RFFromHere: r (non-Maybe)
    ColFI ('Ann ren sch d _) fld ('RFFromHere (toTab :: NameNSK) refs) r =
      '[ 'ColInfo '(fld, 0) (PgTag (AnnRefTabDepth ('Ann ren sch d toTab) toTab) r)
        (ApplyRenamer ren fld) ('RFFromHere toTab refs) ]
    ColFI ('Ann ren sch d _) fld ('RFToHere (fromTab :: NameNSK) refs) [t] =
      '[ 'ColInfo '(fld, 0) [PgTag (AnnRefTabDepth ('Ann ren sch d fromTab) fromTab) t]
        (ApplyRenamer ren fld) ('RFToHere fromTab refs) ]
    ColFI ann fld ('RFSelfRef tab refs) [t] = ColFI ann fld ('RFToHere tab refs) [t]
    ColFI ann fld ('RFSelfRef tab refs) t = ColFI ann fld ('RFFromHere tab refs) t
    ColFI ('Ann ren sch d _) fld fd t = '[ 'ColInfo '(fld, 0) t (ApplyRenamer ren fld) fd ]
--------------------------------------------------------------------------------
-- GCols (closed TF: Generic Rep)
--------------------------------------------------------------------------------

type family GCols ann (rep :: Type -> Type) :: [ColInfo NameNSK] where
    GCols ann (D1 d (C1 c flds)) = Normalize (GCols ann flds)
    GCols ann (a :*: b) = GCols ann a SP.++ GCols ann b
    GCols ann (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) = Col ann fld t
    GCols ann rep = TypeError
      ( Text "Only Product types with fields are supported in pg-schema."
      :$$: Text "(sum types, empty, or missing field selector are not supported)"
      :$$: Text ""
      :$$: Text "But I've got " :<>: ShowType rep )

--------------------------------------------------------------------------------
-- Normalize (SymNat numbering)
--------------------------------------------------------------------------------

type family AddNum (xs :: [ColInfo p]) (cnts :: [SymNat])
  (accCnts :: [SymNat]) :: [ColInfo p]
  where
    AddNum '[] _ _ = '[]
    AddNum ('ColInfo '(s,_) t f fi : xs) '[] accCnts =
      'ColInfo '(s,0) t f fi ': AddNum xs ('(s,0) ': accCnts) '[]
    AddNum ('ColInfo '(s,_) t f fi : xs) ('(s,n) ': rest) accCnts =
      'ColInfo '(s, n+1) t f fi ': AddNum xs (('(s,n+1) ': accCnts) SP.++ rest) '[]
    AddNum ('ColInfo '(s,x) t f fi : xs) ('(s',n) ': rest) accCnts =
      AddNum ('ColInfo '(s,x) t f fi : xs) rest ('(s',n) ': accCnts)

type Normalize xs = AddNum xs '[] '[]

--------------------------------------------------------------------------------
-- ToJSON for PgTag Ann r
--------------------------------------------------------------------------------

instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, ToJSONCols ann colsCase cols r)
  => ToJSON (PgTag ann r) where
    toJSON (PgTag r) = Object (KM.fromList (toPairs @ann @colsCase @cols r))
    toEncoding (PgTag r) = pairs (foldMap (uncurry (.=)) $ toPairs @ann @colsCase @cols r)

instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, FromJSONCols ann colsCase cols r)
  => FromJSON (PgTag ann r) where
   parseJSON v = PgTag <$> parseJSONCols @ann @colsCase @cols v

-- >>> type AnnRel = 'Ann RenamerId PgCatalog (PGC "pg_constraint")
-- >>> rel = PgRelation{ constraint__namespace = PgTag "a", conname = "b", constraint__class = PgClassShort (PgTag "c") "d", constraint__fclass = PgClassShort (PgTag "e") "f", conkey = pgArr' [1,2], confkey = pgArr' [] }
-- >>> rec = "conname" =: ("x" :: T.Text) :. "conname" =: ("z" :: T.Text) :. rel :. "conname" =: ("y" :: T.Text)
-- >>> toJSON $ PgTag @AnnRel rec
-- >>> fromJSON (toJSON $ PgTag @AnnRel rec) == Success (PgTag @AnnRel rec)
-- Object (fromList [("confkey",Array []),("conkey",Array [Number 1.0,Number 2.0]),("conname",String "x"),("conname___1",String "z"),("conname___2",String "b"),("conname___3",String "y"),("constraint__class",Object (fromList [("class__namespace",Object (fromList [("nspname",String "c")])),("relname",String "d")])),("constraint__fclass",Object (fromList [("class__namespace",Object (fromList [("nspname",String "e")])),("relname",String "f")])),("constraint__namespace",Object (fromList [("nspname",String "a")]))])
-- True


class ToJSONCols (ann :: Ann) (colsCase :: ColsCase) (cols :: [ColInfo NameNSK]) r where
  toPairs :: r -> [Pair]

class FromJSONCols (ann :: Ann) (colsCase :: ColsCase) (cols :: [ColInfo NameNSK]) r where
  parseJSONCols :: Value -> Parser r

-- (:=)
--------------------------------------------------------------------------------
-- NonGeneric: (:=) и (:.)
--------------------------------------------------------------------------------
instance ToJSONCols ann 'NonGenericCase '[] (fld := ()) where
  toPairs _ = []

instance FromJSONCols ann 'NonGenericCase '[] (fld := ()) where
  parseJSONCols = pure $ pure (PgTag ())

instance (KnownSymNat sn, ToJSON t, Coercible v t)
  => ToJSONCols ann 'NonGenericCase '[ 'ColInfo sn t db fi ] (fld := v) where
    toPairs (PgTag v) = [Key.fromText (demote @(NameSymNat sn)) .= coerce @_ @t v]

instance
  (KnownSymNat sn, FromJSON tEff, Coercible t tEff)
  => FromJSONCols ann 'NonGenericCase '[ 'ColInfo sn tEff db fi ] (fld := t) where
    parseJSONCols = withObject "record" $ \obj -> do
      case KM.lookup (Key.fromText keyTxt) obj of
        Nothing -> fail ("missing key " ++ T.unpack keyTxt)
        Just v  -> coerce <$> parseJSON @tEff v        -- eff :: tEff
      where
        keyTxt = demote @(NameSymNat sn)

-- Note: we use "split ~" instead of " '(colsA, colsB) ~ " to avoid ambiguity.
instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , split ~ SplitAt (Length (Cols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , ToJSONCols ann ca colsA a, ToJSONCols ann cb colsB b )
  => ToJSONCols ann 'NonGenericCase cols (a :. b) where
    toPairs (a :. b) = toPairs @ann @ca @colsA a <> toPairs @ann @cb @colsB b

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , split ~ SplitAt (Length (Cols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , FromJSONCols ann ca colsA a, FromJSONCols ann cb colsB b)
  => FromJSONCols ann 'NonGenericCase cols (a :. b) where
    parseJSONCols v =
      (:.) <$> parseJSONCols @ann @ca @colsA v <*> parseJSONCols @ann @cb @colsB v

--------------------------------------------------------------------------------
-- Generic
-----------------------------------------------------------------------------
class GToJSONCols (ann :: Ann) (cols :: [ColInfo NameNSK]) (rep :: Type -> Type) where
  gToPairs :: rep x -> [Pair]

class GFromJSONCols (ann :: Ann) (cols :: [ColInfo NameNSK]) (rep :: Type -> Type) where
  gParseJSONCols :: Value -> Parser (rep x)

instance
  (Generic r, GToJSONCols ann cols (Rep r))
  => ToJSONCols ann 'GenericCase cols r where
    toPairs r = gToPairs @ann @cols (from r)

instance
  (Generic r, GFromJSONCols ann cols (Rep r))
  => FromJSONCols ann 'GenericCase cols r where
    parseJSONCols = fmap to . gParseJSONCols @ann @cols

-- D1/C1
instance GToJSONCols ann cols flds
  => GToJSONCols ann cols (D1 d (C1 c flds)) where
    gToPairs (M1 (M1 x)) = gToPairs @ann @cols x

instance GFromJSONCols ann cols flds
  => GFromJSONCols ann cols (D1 d (C1 c flds)) where
    gParseJSONCols = fmap (M1 . M1) . gParseJSONCols @ann @cols

-- (:*:)
instance
  ( split ~ SplitAt (Length (GCols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , GToJSONCols ann colsA a, GToJSONCols ann colsB b )
  => GToJSONCols ann cols (a :*: b) where
    gToPairs (a :*: b) = gToPairs @ann @colsA a <> gToPairs @ann @colsB b

instance
  ( split ~ SplitAt (Length (GCols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , GFromJSONCols ann colsA a, GFromJSONCols ann colsB b )
  => GFromJSONCols ann cols (a :*: b) where
    gParseJSONCols v =
      (:*:) <$> gParseJSONCols @ann @colsA v <*> gParseJSONCols @ann @colsB v

-- Rec0
instance (ToJSONCols ann 'NonGenericCase cols (fld := t))
  => GToJSONCols ann cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t))
  where
    gToPairs (M1 (K1 v)) = toPairs @ann @'NonGenericCase @cols (fld =: v)

instance (FromJSONCols ann 'NonGenericCase cols (fld := t))
  => GFromJSONCols ann cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) where
    gParseJSONCols = fmap (M1 . K1 . unPgTag)
      . parseJSONCols @ann @'NonGenericCase @cols @(fld := t)

--------------------------------------------------------------------------------
-- ToRow / FromRow for PgTag ann r
--------------------------------------------------------------------------------
instance ToJSON (PgTag ann r) => ToField (PgTag (ann :: Ann) r) where
  toField = toJSONField

-- instance ToJSON (PgTag ann r) => ToField (Maybe (PgTag (ann :: Ann) r)) where
--   toField = toJSONField

instance ToJSON (PgTag ann r) => ToField [PgTag (ann :: Ann) r] where
  toField = toJSONField

instance (FromJSON (PgTag ann r), Typeable ann, Typeable r)
  => FromField (PgTag (ann :: Ann) r) where
    fromField = fromJSONField

-- instance (FromJSON (PgTag ann r), Typeable ann, Typeable r)
--   => FromField (Maybe (PgTag (ann :: Ann) r)) where
--     fromField = fromJSONField

instance (FromJSON (PgTag ann r), Typeable ann, Typeable r)
  => FromField [PgTag (ann :: Ann) r] where
    fromField = fromJSONField

--------------------------------------------------------------------------------
-- ToRow / FromRow for PgTag ann r
--------------------------------------------------------------------------------
instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, ToRowCols ann colsCase cols r)
  => ToRow (PgTag ann r) where
    toRow (PgTag r) = toRowCols @ann @colsCase @cols r

instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, FromRowCols ann colsCase cols r)
  => FromRow (PgTag ann r) where
    fromRow = PgTag <$> fromRowCols @ann @colsCase @cols

-- >>> type AnnRel = 'Ann RenamerId PgCatalog (PGC "pg_constraint")
-- >>> (r1 :: [PgTag AnnRel ( ("conkey" := Int16))]) <- query_ conn "select 1::int2"
-- >>> r1
-- [PgTag {unPgTag = PgTag {unPgTag = 1}}]

class ToRowCols (ann :: Ann) (colsCase :: ColsCase) (cols :: [ColInfo NameNSK]) r
  where
    toRowCols :: r -> [Action]

class FromRowCols (ann :: Ann) (colsCase :: ColsCase) (cols :: [ColInfo NameNSK]) r
  where
    fromRowCols :: RowParser r

--------------------------------------------------------------------------------
-- NonGeneric: (:=) and (:.)
--------------------------------------------------------------------------------
instance ToRowCols ann 'NonGenericCase '[] (fld := ()) where
  toRowCols _ = []

instance FromRowCols ann 'NonGenericCase '[] (fld := ()) where
  fromRowCols = pure (PgTag ())

instance (KnownSymNat sn, ToField t, Coercible v t)
  => ToRowCols ann 'NonGenericCase '[ 'ColInfo sn t db fi ] (fld := v) where
    toRowCols (PgTag v) = [toField (coerce @_ @t v)]

instance (FromField tEff, Coercible t tEff)
  => FromRowCols ann 'NonGenericCase '[ 'ColInfo sn tEff db fi ] (fld := t) where
    fromRowCols = coerce <$> field @tEff

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , split ~ SplitAt (Length (Cols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , ToRowCols ann ca colsA a, ToRowCols ann cb colsB b )
  => ToRowCols ann 'NonGenericCase cols (a :. b) where
  toRowCols (a :. b) = toRowCols @ann @ca @colsA a <> toRowCols @ann @cb @colsB b

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , split ~ SplitAt (Length (Cols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , FromRowCols ann ca colsA a, FromRowCols ann cb colsB b )
  => FromRowCols ann 'NonGenericCase cols (a :. b) where
  fromRowCols = (:.)
    <$> fromRowCols @ann @ca @(Fst split)
    <*> fromRowCols @ann @cb @(Snd split)

--------------------------------------------------------------------------------
-- Generic: through Rep r
--------------------------------------------------------------------------------

class GToRowCols (ann :: Ann) (cols :: [ColInfo NameNSK]) (rep :: Type -> Type) where
  gToRowCols :: rep x -> [Action]

class GFromRowCols (ann :: Ann) (cols :: [ColInfo NameNSK]) (rep :: Type -> Type) where
  gFromRowCols :: RowParser (rep x)

instance (Generic r, GToRowCols ann cols (Rep r))
  => ToRowCols (ann :: Ann) 'GenericCase cols r where
    toRowCols r = gToRowCols @ann @cols (from r)

instance (Generic r, GFromRowCols ann cols (Rep r))
  => FromRowCols ann 'GenericCase cols r where
    fromRowCols = to <$> gFromRowCols @ann @cols

instance GToRowCols ann cols flds
  => GToRowCols ann cols (D1 d (C1 c flds)) where
    gToRowCols (M1 (M1 x)) = gToRowCols @ann @cols x

instance GFromRowCols ann cols flds
  => GFromRowCols ann cols (D1 d (C1 c flds)) where
    gFromRowCols = fmap (M1 . M1) (gFromRowCols @ann @cols)

instance
  ( split ~ SplitAt (Length (GCols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , GToRowCols ann colsA a, GToRowCols ann colsB b )
  => GToRowCols ann cols (a :*: b) where
    gToRowCols (a :*: b) = gToRowCols @ann @colsA a <> gToRowCols @ann @colsB b

instance
  ( split ~ SplitAt (Length (GCols ann a)) cols
  , colsA ~ Fst split, colsB ~ Snd split
  , GFromRowCols ann colsA a, GFromRowCols ann colsB b )
  => GFromRowCols ann cols (a :*: b) where
    gFromRowCols = (:*:) <$> gFromRowCols @ann @colsA <*> gFromRowCols @ann @colsB

instance ToRowCols ann 'NonGenericCase cols (fld := t)
  => GToRowCols ann cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) where
    gToRowCols (M1 (K1 v)) = toRowCols @ann @'NonGenericCase @cols (fld =: v)

instance
  FromRowCols ann 'NonGenericCase cols (fld := t)
  => GFromRowCols ann cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) where
    gFromRowCols = M1 . K1 . unPgTag
      <$> fromRowCols @ann @'NonGenericCase @cols @(fld := t)

--------------------------------------------------------------------------------
-- CRecInfo
--------------------------------------------------------------------------------

data FieldInfo s = FieldInfo
  { fieldName   :: s
  , fieldDbName :: s
  , fieldKind   :: RecField' s (RecordInfo s) }
  deriving Show

data RecordInfo s   = RecordInfo
  { tabName :: NameNS' s
  , fields  :: [FieldInfo s] }
  deriving Show

class CRecInfo (ann :: Ann) (r :: Type) where
  getRecordInfo :: RecordInfo T.Text

class CRecInfoCols (ann :: Ann) (cols :: [ColInfo NameNSK]) where
  getFields :: [FieldInfo T.Text]

class CFldInfo (ann :: Ann) (fld :: RecField' Symbol NameNSK) t where
  getFldInfo :: RecField (RecordInfo T.Text)

instance
  (ann ~ 'Ann ren sch d tab, SingI tab, cols ~ Cols ann r, CRecInfoCols ann cols)
  => CRecInfo ann r where
  getRecordInfo = RecordInfo (demote @tab) (getFields @ann @cols)

instance CRecInfoCols ann '[] where getFields = []

instance
  (KnownSymNat sn, KnownSymbol db, CFldInfo ann fi t, CRecInfoCols ann cols)
  => CRecInfoCols ann ('ColInfo sn t db fi ': cols) where
  getFields = FieldInfo
    { fieldName   = demote @(NameSymNat sn)
    , fieldDbName = demote @db
    , fieldKind   = getFldInfo @ann @fi @t } : getFields @ann @cols

instance ToStar fd => CFldInfo ann ('RFPlain fd) t where
  getFldInfo = RFPlain (demote @fd)

instance (ToStar fd, ToStar af, ToStar b) =>
  CFldInfo ann ('RFAggr fd af b) t where
  getFldInfo = RFAggr (demote @fd) (demote @af) (demote @b)

type family AnnRefTabDepth (ann :: Ann) refTab :: Ann where
  AnnRefTabDepth ('Ann ren sch d tab) refTab =
    'Ann ren sch (DecDepth ('Ann ren sch d tab)) refTab

instance (CRecInfo ann' r, ToStar refs, ann' ~ AnnRefTabDepth ann fromTab)
  => CFldInfo ann ('RFToHere fromTab refs) [PgTag ann' r] where
  getFldInfo = RFToHere (getRecordInfo @ann' @r) (demote @refs)

instance (CRecInfo ann' r, ToStar refs, ann' ~ AnnRefTabDepth ann toTab)
  => CFldInfo ann ('RFFromHere toTab refs) (Maybe (PgTag ann' r)) where
  getFldInfo = RFFromHere (getRecordInfo @ann' @r) (demote @refs)

instance (CRecInfo ann' r, ToStar refs, ann' ~ AnnRefTabDepth ann toTab)
  => CFldInfo ann ('RFFromHere toTab refs) (PgTag ann' r) where
  getFldInfo = RFFromHere (getRecordInfo @ann' @r) (demote @refs)

--------------------------------------------------------------------------------
-- Helpers over Cols ann r
--------------------------------------------------------------------------------

type family ColDbName (c :: ColInfo p) :: Symbol where
  ColDbName ('ColInfo '(fld, idx) t db fi) = db

type family ColsDbNames (cols :: [ColInfo p]) :: [Symbol] where
  ColsDbNames '[]       = '[]
  ColsDbNames (c ': cs) = ColDbName c ': ColsDbNames cs

--------------------------------------------------------------------------------
-- Plain fields (without relation fields)
--------------------------------------------------------------------------------

type family IsPlainRecField (fi :: RecField' Symbol NameNSK) :: Bool where
  IsPlainRecField ('RFToHere tab rs)   = 'False
  IsPlainRecField ('RFFromHere tab rs) = 'False
  IsPlainRecField ('RFSelfRef tab rs)  = 'False
  IsPlainRecField fi        = 'True

type family AllPlainCols (cols :: [ColInfo NameNSK]) :: Bool where
  AllPlainCols '[] = 'True
  AllPlainCols ('ColInfo sn t db fi ': cs) = IsPlainRecField fi && AllPlainCols cs

-- | All fields are plain (no RFToHere/RFFromHere)
type family AllPlain (ann :: Ann) (r :: Type) :: Constraint where
  AllPlain ann r = Assert (AllPlainCols (Cols ann r))
    (TypeError
      (  Text "Not all fields in record are 'plain' (no relations allowed)."
      :$$: Text "Ann:   " :<>: ShowType ann
      :$$: Text "Type:  " :<>: ShowType r
      :$$: Text "Cols:  " :<>: ShowType (Cols ann r) ))

--------------------------------------------------------------------------------
-- Type-level RecordInfo for Ann
--------------------------------------------------------------------------------
-- | Decrease relation-walk depth kept in 'Ann'.
-- When depth is exhausted, fail with a detailed type error instead of
-- potentially diverging in recursive type families/instances.
type family DecDepth (ann :: Ann) :: Nat where
  DecDepth ('Ann ren sch 0 tab) = TypeError
    (  Text "pg-schema: relation walk depth limit reached."
    :$$: Text ""
    :$$: Text "Ann:   " :<>: ShowType ('Ann ren sch 0 tab)
    :$$: Text "Table: " :<>: ShowType tab
    :$$: Text ""
    :$$: Text "Likely reason:"
    :$$: Text "  Recursive/self-referential tree (SelfRef or cycle) is deeper"
    :$$: Text "  than annDepth in your Ann."
    :$$: Text ""
    :$$: Text "How to fix:"
    :$$: Text "  1) Increase annDepth in Ann;"
    :$$: Text "  2) Reduce recursion depth in selected/inserted shape;"
    :$$: Text "  3) For true graph cycles, use manual SQL." )
  DecDepth ('Ann _ _ d _) = d - 1

-- Type-level analogue of CFldInfo: take DB-level RecFieldK and Haskell type of field t
-- and build RecField' Symbol (RecordInfo Symbol) with TRecordInfo for children.
type family TFldInfo (ann :: Ann) (fi :: RecField' Symbol NameNSK) t
  :: RecField' Symbol (RecordInfo Symbol) where
  TFldInfo ann ('RFPlain fd) t      = 'RFPlain fd
  TFldInfo ann ('RFAggr  fd af b) t = 'RFAggr fd af b
  TFldInfo ann ('RFEmpty s)    t    = 'RFEmpty s
  TFldInfo ('Ann ren sch d tab) ('RFToHere (toTab :: NameNSK) refs)
    [PgTag ('Ann ren sch d' toTab) rChild] =
    'RFToHere ('RecordInfo toTab (TRecordInfo ('Ann ren sch d' toTab) rChild)) refs
  TFldInfo ('Ann ren sch d tab) ('RFFromHere (toTab :: NameNSK) refs)
    (Maybe (PgTag ('Ann ren sch d' toTab) rChild)) =
    'RFFromHere ('RecordInfo toTab (TRecordInfo ('Ann ren sch d' toTab) rChild)) refs
  TFldInfo ('Ann ren sch d tab) ('RFFromHere (toTab :: NameNSK) refs)
    (PgTag ('Ann ren sch d' toTab) rChild) =
    'RFFromHere ('RecordInfo toTab (TRecordInfo ('Ann ren sch d' toTab) rChild)) refs
  TFldInfo ann fi t = TypeError
    (  Text "TFldInfo: unsupported RecField for Ann."
    :$$: Text "  Ann: " :<>: ShowType ann
    :$$: Text "  RecField: " :<>: ShowType fi
    :$$: Text "  Haskell type: " :<>: ShowType t
    :$$: Text ""
    :$$: Text "Most likely TDBFieldInfo / ColFI produced a constructor"
    :$$: Text "that TFldInfo does not know how to map into RecordInfo." )

type family TRecordInfoCols (ann  :: Ann) (cols :: [ColInfo NameNSK]) :: [FieldInfo Symbol] where
  TRecordInfoCols ann '[] = '[]
  TRecordInfoCols ann ('ColInfo sn t db fi ': cs) =
    'FieldInfo (NameSymNat sn) db (TFldInfo ann fi t) ': TRecordInfoCols ann cs

type family TRecordInfo (ann :: Ann) (r :: Type) :: [FieldInfo Symbol] where
  TRecordInfo ann r = TRecordInfoCols ann (Cols ann r)

--------------------------------------------------------------------------------
-- Node-level checks for Mandatory / PK (analogue of CheckNodeAll*)
--------------------------------------------------------------------------------

-- | One-table check that all mandatory fields are present
-- rs: list of columns that are already "covered" (including those that come from Reference)
type family CheckAllMandatory (ann :: Ann) (rs :: [Symbol]) :: Constraint where
  CheckAllMandatory ('Ann ren sch d tab) rs = TE.Assert
    (SP.Null (RestMandatory sch tab rs))
    (TypeError
      (  Text "We can't insert data because not all mandatory fields are present."
      :$$: Text "Table: " :<>: ShowType tab
      :$$: Text "Missing mandatory fields: " :<>: ShowType (RestMandatory sch tab rs) ))

-- | One-table check that all mandatory fields are present
-- or all PK fields are present
type family CheckAllMandatoryOrHasPK (ann :: Ann) (rs :: [Symbol]) :: Constraint where
  CheckAllMandatoryOrHasPK ('Ann ren sch d tab) rs = TE.Assert
    ( SP.Null (RestMandatory sch tab rs)
      || SP.Null (RestPK sch tab rs) )
    (TypeError
      (  Text "We can't upsert data because for table " :<>: ShowType tab
      :$$: Text "either not all mandatory fields or not all PK fields are present."
      :$$: Text "Missing mandatory fields: " :<>: ShowType (RestMandatory sch tab rs)
      :$$: Text "Missing PK fields: " :<>: ShowType (RestPK sch tab rs) ))

genDefunSymbols [ ''CheckAllMandatory, ''CheckAllMandatoryOrHasPK]

--------------------------------------------------------------------------------
-- Recursive AllMandatory / PK for tree (JSON insert / upsert)
--------------------------------------------------------------------------------
type family WalkLevelAnn
  (check :: Ann ~> [Symbol] ~> Constraint)
  (ann :: Ann) (fis :: [FieldInfo Symbol]) (rs :: [Symbol]) :: Constraint where
  WalkLevelAnn check ann '[] rs = SP.Apply (SP.Apply check ann) rs
  WalkLevelAnn check ann ('FieldInfo name db ('RFPlain fd) ': xs) rs =
    WalkLevelAnn check ann xs (db ': rs)
  WalkLevelAnn check ('Ann ren sch d tab)
    ('FieldInfo _ _ ('RFToHere ('RecordInfo childTab childFIs) refs) ': xs) rs =
      ( WalkLevelAnn check ('Ann ren sch d childTab) childFIs (SP.Map1 FromNameSym0 refs)
      , WalkLevelAnn check ('Ann ren sch d tab) xs rs )
  WalkLevelAnn check ann (_ ': xs) rs = WalkLevelAnn check ann xs rs

type family AllMandatoryTree (ann :: Ann) (r :: Type) (rFlds :: [Symbol])
  :: Constraint where
  AllMandatoryTree ann [r] rFlds = AllMandatoryTree ann r rFlds
  AllMandatoryTree ann r rFlds =
    WalkLevelAnn CheckAllMandatorySym0 ann (TRecordInfo ann r) rFlds

type family AllMandatoryOrHasPKTree (ann :: Ann) (r :: Type) (rFlds :: [Symbol])
  :: Constraint where
  AllMandatoryOrHasPKTree ann [r] rFlds = AllMandatoryOrHasPKTree ann r rFlds
  AllMandatoryOrHasPKTree ann r rFlds =
    WalkLevelAnn CheckAllMandatoryOrHasPKSym0 ann (TRecordInfo ann r) rFlds

--------------------------------------------------------------------------------
-- Returning tree must be subtree of input tree (with path)
--------------------------------------------------------------------------------

type family Snoc (p :: [k]) (x :: k) :: [k] where
  Snoc '[] x = '[x]
  Snoc (a ': as) x = a ': Snoc as x

type family FindChildAt (path :: [Symbol]) (db :: Symbol)
  (fisIn :: [FieldInfo Symbol]) :: [FieldInfo Symbol] where
  FindChildAt path db '[] = TypeError
    (  Text "Returning tree is not a subtree of input tree."
    :$$: Text "At path: " :<>: ShowType path
    :$$: Text "Missing branch (db name): " :<>: ShowType db )
  FindChildAt _ db ('FieldInfo _ db ('RFToHere ('RecordInfo _ fis) _) ': xs) = fis
  FindChildAt _ db ('FieldInfo _ db ('RFFromHere ('RecordInfo _ fis) _) ': xs) = fis
  FindChildAt path db (_ ': xs) = FindChildAt path db xs

type family CheckSubtreeAt (path :: [Symbol]) (fisIn :: [FieldInfo Symbol])
  (fisOut :: [FieldInfo Symbol]) :: Constraint where
  CheckSubtreeAt path fisIn '[] = ()
  CheckSubtreeAt path fisIn
    ('FieldInfo _ db ('RFToHere ('RecordInfo _ childFIsOut) _) ': xs) =
      ( CheckSubtreeAt (Snoc path db) (FindChildAt path db fisIn) childFIsOut
      , CheckSubtreeAt path fisIn xs )
  CheckSubtreeAt path fisIn
    ('FieldInfo _ db ('RFFromHere ('RecordInfo _ childFIsOut) _) ': xs) =
      ( CheckSubtreeAt (Snoc path db) (FindChildAt path db fisIn) childFIsOut
      , CheckSubtreeAt path fisIn xs )
  CheckSubtreeAt path fisIn (_ ': xs) = CheckSubtreeAt path fisIn xs

-- | Check that returning tree is a subtree of input tree
type family ReturningIsSubtree (ann :: Ann) (rIn :: Type) (rOut :: Type) :: Constraint where
  ReturningIsSubtree ann rIn rOut =
    CheckSubtreeAt '[] (TRecordInfo ann rIn) (TRecordInfo ann rOut)
