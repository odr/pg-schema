{-# LANGUAGE UndecidableInstances #-}
module PgSchema.Ann where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Data.Coerce
import Data.Singletons.TH (genDefunSymbols)
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
import GHC.TypeError
import PgSchema.Schema
import PgSchema.Types
import PgSchema.Utils.Internal
import Prelude.Singletons as SP hiding (type (+))



-- $setup
-- >>> import PgSchema.Schema.Catalog
-- >>> import Database.PostgreSQL.Simple
-- >>> conn <- connectPostgreSQL ""

data Ann = Ann
  { annRen  :: Symbol ~> Symbol
  , annSch  :: Type
  , annTab  :: NameNSK }

data ColInfo (p :: Type) = ColInfo
  { ciField   :: SymNat
  , ciType    :: Type
  , ciDbField :: Symbol
  , ciInfo    :: RecField' Symbol p
  }

type Renamer = Symbol ~> Symbol
data RenamerId :: Renamer
type instance Apply RenamerId s = s

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
  Col ('Ann ren sch tab) fld t = ColFI ('Ann ren sch tab) fld (TDBFieldInfo sch tab (Apply ren fld)) t

type family ColFI (ann :: Ann) (fld :: Symbol) (fi :: RecFieldK NameNSK) t
  :: [ColInfo NameNSK] where
    ColFI ('Ann ren sch _) fld ('RFPlain ('FldDef tn False def)) (PgArr t) =
      '[ 'ColInfo '(fld, 0) (Tagged (TypElem (TTypDef sch tn)) (PgArr t))
        (Apply ren fld) (RFPlain ('FldDef tn False def))]
    ColFI ('Ann ren sch _) fld ('RFPlain ('FldDef tn True def)) (Maybe (PgArr t)) =
      '[ 'ColInfo '(fld, 0) (Maybe (Tagged (TypElem (TTypDef sch tn)) (PgArr t)))
        (Apply ren fld) (RFPlain ('FldDef tn True def))]
    -- RFFromHere: Maybe r
    ColFI ('Ann ren sch _) fld ('RFFromHere (toTab :: NameNSK) refs) (Maybe r) =
      '[ 'ColInfo '(fld, 0) (Maybe (Tagged ('Ann ren sch toTab) r))
        (Apply ren fld) ('RFFromHere toTab refs) ]
    -- RFFromHere: r (non-Maybe)
    ColFI ('Ann ren sch _) fld ('RFFromHere (toTab :: NameNSK) refs) r =
      '[ 'ColInfo '(fld, 0) (Tagged ('Ann ren sch toTab) r)
        (Apply ren fld) ('RFFromHere toTab refs) ]
    ColFI ('Ann ren sch _) fld ('RFToHere (fromTab :: NameNSK) refs) [t] =
      '[ 'ColInfo '(fld, 0) [Tagged ('Ann ren sch fromTab) t]
        (Apply ren fld) ('RFToHere fromTab refs) ]
    ColFI ('Ann ren sch _) fld fd t = '[ 'ColInfo '(fld, 0) t (Apply ren fld) fd ]

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
-- ToJSON for Tagged Ann r
--------------------------------------------------------------------------------

instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, ToJSONCols ann colsCase cols r)
  => ToJSON (Tagged ann r) where
    toJSON (Tagged r) = Object (KM.fromList (toPairs @ann @colsCase @cols r))
    toEncoding (Tagged r) = pairs (foldMap (uncurry (.=)) $ toPairs @ann @colsCase @cols r)

instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, FromJSONCols ann colsCase cols r)
  => FromJSON (Tagged ann r) where
   parseJSON v = Tagged <$> parseJSONCols @ann @colsCase @cols v

-- >>> type AnnRel = 'Ann RenamerId PgCatalog (PGC "pg_constraint")
-- >>> rel = PgRelation{ constraint__namespace = Tagged "a", conname = "b", constraint__class = PgClassShort (Tagged "c") "d", constraint__fclass = PgClassShort (Tagged "e") "f", conkey = pgArr' [1,2], confkey = pgArr' [] }
-- >>> rec = "conname" =: ("x" :: T.Text) :. "conname" =: ("z" :: T.Text) :. rel :. "conname" =: ("y" :: T.Text)
-- >>> toJSON $ Tagged @AnnRel rec
-- >>> fromJSON (toJSON $ Tagged @AnnRel rec) == Success (Tagged @AnnRel rec)
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

-- Если в Ann поле имеет тип () — оно мапится в пустой набор колонок.
-- Тогда ToRow/FromRow должны просто не трогать БД и возвращать Tagged ().
instance ToJSONCols ann 'NonGenericCase '[] (fld := ()) where
  toPairs _ = []

instance FromJSONCols ann 'NonGenericCase '[] (fld := ()) where
  parseJSONCols = pure $ pure (Tagged ())

instance (KnownSymNat sn, ToJSON t, Coercible v t)
  => ToJSONCols ann 'NonGenericCase '[ 'ColInfo sn t db fi ] (fld := v) where
    toPairs (Tagged v) = [Key.fromText (demote @(NameSymNat sn)) .= coerce @_ @t v]

instance
  (KnownSymNat sn, FromJSON tEff, Coercible t tEff)
  => FromJSONCols ann 'NonGenericCase '[ 'ColInfo sn tEff db fi ] (fld := t) where
    parseJSONCols = withObject "record" $ \obj -> do
      case KM.lookup (Key.fromText keyTxt) obj of
        Nothing -> fail ("missing key " ++ T.unpack keyTxt)
        Just v  -> coerce <$> parseJSON @tEff v        -- eff :: tEff
      where
        keyTxt = demote @(NameSymNat sn)

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , '(colsA, colsB) ~ SplitAt (Length (Cols ann a)) cols
  , ToJSONCols ann ca colsA a, ToJSONCols ann cb colsB b )
  => ToJSONCols ann 'NonGenericCase cols (a :. b) where
    toPairs (a :. b) = toPairs @ann @ca @colsA a <> toPairs @ann @cb @colsB b

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , '(colsA, colsB) ~ SplitAt (Length (Cols ann a)) cols
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
  ( '(colsA, colsB) ~ SplitAt (Length (GCols ann a)) cols
  , GToJSONCols ann colsA a, GToJSONCols ann colsB b )
  => GToJSONCols ann cols (a :*: b) where
    gToPairs (a :*: b) = gToPairs @ann @colsA a <> gToPairs @ann @colsB b

instance
  ( '(colsA, colsB) ~ SplitAt (Length (GCols ann a)) cols
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
    gParseJSONCols = fmap (M1 . K1 . unTagged)
      . parseJSONCols @ann @'NonGenericCase @cols @(fld := t)

--------------------------------------------------------------------------------
-- ToRow / FromRow для Tagged ann r
--------------------------------------------------------------------------------
instance ToJSON (Tagged ann r) => ToField (Tagged (ann :: Ann) r) where
  toField = toJSONField

instance ToJSON (Tagged ann r) => ToField [Tagged (ann :: Ann) r] where
  toField = toJSONField

instance (FromJSON (Tagged ann r), Typeable ann, Typeable r)
  => FromField (Tagged (ann :: Ann) r) where
    fromField = fromJSONField

instance (FromJSON (Tagged ann r), Typeable ann, Typeable r)
  => FromField [Tagged (ann :: Ann) r] where
    fromField = fromJSONField

--------------------------------------------------------------------------------
-- ToRow / FromRow для Tagged ann r
--------------------------------------------------------------------------------
instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, ToRowCols ann colsCase cols r)
  => ToRow (Tagged ann r) where
    toRow (Tagged r) = toRowCols @ann @colsCase @cols r

instance
  (cols ~ Cols ann r, colsCase ~ ColsCaseOf r, FromRowCols ann colsCase cols r)
  => FromRow (Tagged ann r) where
    fromRow = Tagged <$> fromRowCols @ann @colsCase @cols

-- >>> type AnnRel = 'Ann RenamerId PgCatalog (PGC "pg_constraint")
-- >>> (r1 :: [Tagged AnnRel ( ("conkey" := Int16))]) <- query_ conn "select 1::int2"
-- >>> r1
-- [Tagged {unTagged = Tagged {unTagged = 1}}]

class ToRowCols (ann :: Ann) (colsCase :: ColsCase) (cols :: [ColInfo NameNSK]) r
  where
    toRowCols :: r -> [Action]

class FromRowCols (ann :: Ann) (colsCase :: ColsCase) (cols :: [ColInfo NameNSK]) r
  where
    fromRowCols :: RowParser r

--------------------------------------------------------------------------------
-- NonGeneric: (:=) и (:.)
--------------------------------------------------------------------------------
instance ToRowCols ann 'NonGenericCase '[] (fld := ()) where
  toRowCols _ = []

instance FromRowCols ann 'NonGenericCase '[] (fld := ()) where
  fromRowCols = pure (Tagged ())

instance (KnownSymNat sn, ToField t, Coercible v t)
  => ToRowCols ann 'NonGenericCase '[ 'ColInfo sn t db fi ] (fld := v) where
    toRowCols (Tagged v) = [toField (coerce @_ @t v)]

instance (FromField tEff, Coercible t tEff)
  => FromRowCols ann 'NonGenericCase '[ 'ColInfo sn tEff db fi ] (fld := t) where
    fromRowCols = coerce <$> field @tEff

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , '(colsA, colsB) ~ SplitAt (Length (Cols ann a)) cols
  , ToRowCols ann ca colsA a, ToRowCols ann cb colsB b )
  => ToRowCols ann 'NonGenericCase cols (a :. b) where
  toRowCols (a :. b) = toRowCols @ann @ca @colsA a <> toRowCols @ann @cb @colsB b

instance
  ( ca ~ ColsCaseOf a, cb ~ ColsCaseOf b
  , '(colsA, colsB) ~ SplitAt (Length (Cols ann a)) cols
  , FromRowCols ann ca colsA a, FromRowCols ann cb colsB b )
  => FromRowCols ann 'NonGenericCase cols (a :. b) where
  fromRowCols = (:.) <$> fromRowCols @ann @ca @colsA <*> fromRowCols @ann @cb @colsB

--------------------------------------------------------------------------------
-- Generic: через Rep r
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
  ( '(colsA, colsB) ~ SplitAt (Length (GCols ann a)) cols
  , GToRowCols   ann colsA a, GToRowCols   ann colsB b )
  => GToRowCols ann cols (a :*: b) where
    gToRowCols (a :*: b) = gToRowCols @ann @colsA a <> gToRowCols @ann @colsB b

instance
  ( '(colsA, colsB) ~ SplitAt (Length (GCols ann a)) cols
  , GFromRowCols ann colsA a, GFromRowCols ann colsB b )
  => GFromRowCols ann cols (a :*: b) where
    gFromRowCols = (:*:) <$> gFromRowCols @ann @colsA <*> gFromRowCols @ann @colsB

instance ToRowCols ann 'NonGenericCase cols (fld := t)
  => GToRowCols ann cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) where
    gToRowCols (M1 (K1 v)) = toRowCols @ann @'NonGenericCase @cols (fld =: v)

instance
  FromRowCols ann 'NonGenericCase cols (fld := t)
  => GFromRowCols ann cols (S1 (MetaSel ('Just fld) u v w) (Rec0 t)) where
    gFromRowCols = M1 . K1 . unTagged
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

-- instance (ann ~ 'Ann ren sch tab, SingI tab) => CRecInfo ann () where
--   getRecordInfo = RecordInfo (demote @tab) []

instance
  (ann ~ 'Ann ren sch tab, SingI tab, cols ~ Cols ann r, CRecInfoCols ann cols)
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

-- -- "пустое" поле должно быть исключено из CFldInfo раньше
-- instance (KnownSymbol s) => CFldInfo ann ('RFEmpty s) t where
--   getFldInfo = RFEmpty $ demote @s

-- агрегат
instance (ToStar fd, ToStar af, ToStar b) =>
  CFldInfo ann ('RFAggr fd af b) t where
  getFldInfo = RFAggr (demote @fd) (demote @af) (demote @b)

instance (CRecInfo ('Ann ren sch fromTab) r, ToStar refs)
  => CFldInfo ('Ann ren sch tab) ('RFToHere fromTab refs)
    [Tagged ('Ann ren sch fromTab) r] where
  getFldInfo = RFToHere (getRecordInfo @('Ann ren sch fromTab) @r) (demote @refs)

-- nullable ребёнок
instance (CRecInfo ('Ann ren sch toTab) r, ToStar refs)
  => CFldInfo ('Ann ren sch tab) ('RFFromHere toTab refs)
    (Maybe (Tagged ('Ann ren sch toTab) r)) where
  getFldInfo = RFFromHere (getRecordInfo @('Ann ren sch toTab) @r) (demote @refs)

-- не‑nullable ребёнок
instance (CRecInfo ('Ann ren sch toTab) r, ToStar refs)
  => CFldInfo ('Ann ren sch tab) ('RFFromHere toTab refs)
    (Tagged ('Ann ren sch toTab) r) where
  getFldInfo = RFFromHere (getRecordInfo @('Ann ren sch toTab) @r) (demote @refs)

--------------------------------------------------------------------------------
-- Helpers over Cols ann r
--------------------------------------------------------------------------------

-- Имя колонки в БД (после Renamer)
type family ColDbName (c :: ColInfo p) :: Symbol where
  ColDbName ('ColInfo '(fld, idx) t db fi) = db

-- Список имён всех колонок (DB names)
type family ColsDbNames (cols :: [ColInfo p]) :: [Symbol] where
  ColsDbNames '[]       = '[]
  ColsDbNames (c ': cs) = ColDbName c ': ColsDbNames cs

--------------------------------------------------------------------------------
-- Plain-поля (без relation-полей)
--------------------------------------------------------------------------------

type family IsPlainRecField (fi :: RecField' Symbol NameNSK) :: Bool where
  IsPlainRecField ('RFToHere tab rs)   = 'False
  IsPlainRecField ('RFFromHere tab rs) = 'False
  IsPlainRecField fi        = 'True

type family AllPlainCols (cols :: [ColInfo NameNSK]) :: Bool where
  AllPlainCols '[] = 'True
  AllPlainCols ('ColInfo sn t db fi ': cs) = IsPlainRecField fi && AllPlainCols cs

-- Все поля plain (нет RFToHere/RFFromHere)
type family AllPlain (ann :: Ann) (r :: Type) :: Constraint where
  AllPlain ann r = Assert (AllPlainCols (Cols ann r))
    (TypeError
      (  Text "Not all fields in record are 'plain' (no relations allowed)."
      :$$: Text "Ann:   " :<>: ShowType ann
      :$$: Text "Type:  " :<>: ShowType r
      :$$: Text "Cols:  " :<>: ShowType (Cols ann r) ))

--------------------------------------------------------------------------------
-- Node-level проверки Mandatory / PK (аналог CheckNodeAll*)
--------------------------------------------------------------------------------

-- rs: список колонок, которые уже "покрыты" (включая те, что придут из Reference)
type family CheckAllMandatory (ann :: Ann) (rs :: [Symbol]) :: Constraint where
  CheckAllMandatory ('Ann ren sch tab) rs = Assert
    (SP.Null (RestMandatory sch tab rs))
    (TypeError
      (  Text "We can't insert data because not all mandatory fields are present."
      :$$: Text "Table: " :<>: ShowType tab
      :$$: Text "Missing mandatory fields: " :<>: ShowType (RestMandatory sch tab rs) ))

type family CheckAllMandatoryOrHasPK (ann :: Ann) (rs :: [Symbol]) :: Constraint where
  CheckAllMandatoryOrHasPK ('Ann ren sch tab) rs = Assert
    ( SP.Null (RestMandatory sch tab rs)
      || SP.Null (RestPK sch tab rs) )
    (TypeError
      (  Text "We can't upsert data because for table " :<>: ShowType tab
      :$$: Text "either not all mandatory fields or not all PK fields are present."
      :$$: Text "Missing mandatory fields: " :<>: ShowType (RestMandatory sch tab rs)
      :$$: Text "Missing PK fields: " :<>: ShowType (RestPK sch tab rs) ))

genDefunSymbols [ ''CheckAllMandatory, ''CheckAllMandatoryOrHasPK]
--------------------------------------------------------------------------------
-- Type-level RecordInfo для Ann
--------------------------------------------------------------------------------

-- Type-level аналог CFldInfo: берём DB-уровневый RecFieldK и Haskell-тип поля t
-- и строим RecField' Symbol (RecordInfo Symbol) с уже вложенным TRecordInfo для детей.
type family TFldInfo (ann :: Ann) (fi :: RecField' Symbol NameNSK) t
  :: RecField' Symbol (RecordInfo Symbol) where
  TFldInfo ann ('RFPlain fd) t      = 'RFPlain fd
  TFldInfo ann ('RFAggr  fd af b) t = 'RFAggr fd af b
  TFldInfo ann ('RFEmpty s)    t    = 'RFEmpty s
  TFldInfo ('Ann ren sch tab) ('RFToHere (toTab :: NameNSK) refs)
    [Tagged ('Ann ren sch toTab) rChild] =
    'RFToHere ('RecordInfo toTab (TRecordInfo ('Ann ren sch toTab) rChild)) refs
  TFldInfo ('Ann ren sch tab) ('RFFromHere (toTab :: NameNSK) refs)
    (Maybe (Tagged ('Ann ren sch toTab) rChild)) =
    'RFFromHere ('RecordInfo toTab (TRecordInfo ('Ann ren sch toTab) rChild)) refs
  TFldInfo ('Ann ren sch tab) ('RFFromHere (toTab :: NameNSK) refs)
    (Tagged ('Ann ren sch toTab) rChild) =
    'RFFromHere ('RecordInfo toTab (TRecordInfo ('Ann ren sch toTab) rChild)) refs
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
-- Recursive AllMandatory / PK for tree (JSON‑insert / upsert)
--------------------------------------------------------------------------------
type family WalkLevelAnn
  (check :: Ann ~> [Symbol] ~> Constraint)
  (ann :: Ann) (fis :: [FieldInfo Symbol]) (rs :: [Symbol]) :: Constraint where
  WalkLevelAnn check ann '[] rs = SP.Apply (SP.Apply check ann) rs
  WalkLevelAnn check ann ('FieldInfo name db ('RFPlain fd) ': xs) rs =
    WalkLevelAnn check ann xs (name ': rs)
  WalkLevelAnn check ('Ann ren sch tab)
    ('FieldInfo _ _ ('RFToHere ('RecordInfo childTab childFIs) refs) ': xs) rs =
      ( WalkLevelAnn check ('Ann ren sch childTab)
          childFIs (SP.Map FromNameSym0 refs)
      , WalkLevelAnn check ('Ann ren sch tab) xs rs )
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
