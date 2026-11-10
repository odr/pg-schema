{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module PgSchema.Types(
  -- * PgTag types
  type (:=), (=:), (:.)(..), PgTag(..), named, Named
  -- * Enum
  , PGEnum
  -- * Aggregates
  , Aggr(..), Aggr'(..)
  -- * Arrays
  , PgArr(..), pgArr', unPgArr'
  -- * Other types
  , PgChar(..), PgOid(..)
  -- * Conversion checks
  , CanConvert, CanConvert1, CheckAggr
  -- * UnsafeCol
  , UnsafeCol(..) )
  where

import Control.Monad
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
#ifdef MK_CASE_INSENSITIVE
import Data.CaseInsensitive
#endif
import Data.Coerce
import Data.Fixed
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Scientific
import Data.Singletons.TH
import Data.Text as T
import Data.Text.Encoding as T
#ifdef MK_TIME
import Data.Time
#endif
#ifdef MK_UUID
import Data.UUID.Types
#endif
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.TypeLits as TL
import GHC.TypeError as TE
import GHC.Int
import Prelude as P
import Type.Reflection
import PgSchema.Schema.Catalog (PGC)
import Data.Type.Bool (If, Not)
import PgSchema.Schema
import PgSchema.Utils.Internal hiding (fromText)
import PgSchema.Utils.TF

#ifdef MK_ARBITRARY
import Test.QuickCheck(Arbitrary(arbitrary), arbitraryBoundedEnum)
#endif
#ifdef MK_FLAT
import Flat as F
#endif
#ifdef MK_HASHABLE
import Data.Hashable
#endif


-- | Representation of `enum` database types.
--
-- Data instances are produced by schema generation.
-- You can use these data instances in your records to @SELECT@ | @INSERT@ | @UPSERT@ data.
data family PGEnum sch (name :: NameNSK) :: Type

instance
  (Read (PGEnum sch n), ToStar n, Typeable sch, Typeable n)
  => FromField (PGEnum sch n) where
  fromField f mbs =
    case mbs >>= enumFromText . decodeUtf8 of
      Just x -> pure x
      _      -> returnError Incompatible f ""

instance
  (Show (PGEnum sch n), ToStar n) => ToField (PGEnum sch n) where
  toField = toField . enumToText

instance (Read (PGEnum sch t), ToStar t) => FromJSON (PGEnum sch t) where
    parseJSON = parseJSON >=> maybe mzero pure . enumFromText

instance (Show (PGEnum sch t), ToStar t) => ToJSON (PGEnum sch t) where
  toJSON = toJSON . enumToText

enumFromText
  :: forall sch t. (Read (PGEnum sch t), ToStar t)
  => Text -> Maybe (PGEnum sch t)
enumFromText t = fmap fst $ listToMaybe
  $ reads $ T.unpack $ toTitle (nnsName $ demote @t) <> "_" <> t

enumToText :: forall sch t. (Show (PGEnum sch t), ToStar t) => PGEnum sch t -> Text
enumToText = T.drop (T.length (nnsName $ demote @t) + 1) . T.show

#ifdef MK_ARBITRARY
instance (Bounded (PGEnum sch t), Enum (PGEnum sch t)) =>
  Arbitrary (PGEnum sch t) where
    arbitrary = arbitraryBoundedEnum
#endif

#ifdef MK_FLAT
instance (Read (PGEnum sch n), Show (PGEnum sch n)) => Flat (PGEnum sch n) where
  encode = F.encode . P.show
  decode = read <$> F.decode
  size = F.size . P.show
#endif

-- | Representation of aggregate functions.
--
-- E.g. @\"fld\" := Aggr AMin (Maybe Int32)@ means "minimum value of the field `fld`"
--
-- 'Aggr' requires a 'Maybe' argument for all functions except @count@.
--
-- Only a small set of aggregations are supported currently: @count@, @min@, @max@, @sum@, @avg@.
--
-- The Haskell type is checked at compile time against the SQL result type of
-- the aggregate ('CheckAggr'):
--
-- * @count@ - @int8@: 'Int64';
-- * @min@ / @max@ - type of the field (numeric, text, bool or date fields);
-- * @sum(int2 | int4)@ - @int8@: 'Int64', 'Scientific' or 'Rational';
-- * @sum(int8 | numeric)@, @avg(int2 | int4 | int8 | numeric)@ - @numeric@:
--   'Scientific' or 'Rational';
-- * @sum(float4)@, @sum(float8)@, @avg(float4 | float8)@ - @float4@ / @float8@:
--   'Double', 'Scientific' or 'Rational'.
newtype Aggr (f :: AggrFun) t = Aggr { unAggr :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

-- | All aggregate functions except @count@ can return NULL.
-- But if field under aggregate is mandatory they return NULL only on empty set
-- if there is no group by clause. E.g. `select min(a) from t where false`
-- So we require Nullable for Aggr.
--
-- 'Aggr'' is like 'Aggr' but cannot be used in SELECT without `group by`.
-- So it is mandatory if field is mandatory.
newtype Aggr' (f :: AggrFun) t = Aggr' { unAggr' :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

-- | 'Char' has no 'ToField' instance; this is a custom wrapper.
newtype PgChar = PgChar { unPgChar :: Char }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, FromField, Enum, Bounded, FromJSON, ToJSON
#ifdef MK_HASHABLE
    , Hashable )
#else
    )
#endif

instance ToField PgChar where
  toField = toField . (:[]) . unPgChar

--
-- | 'PGArray' has no JSON instances. '[]' has JSON, but no PG instances.
-- This one has both.
--
-- Use this type to work with arrays in database.
--
-- All elements are 'Maybe' because PostgreSQL does not guarantee that all elements are present.
-- 'PgTag' @typeName@ 'PgArr' can be /safely/ converted to 'ToField' with type information (e.g. @val::int[]@).
-- This instance is used internally in the generation of SQL.
--
newtype PgArr a = PgArr { unPgArr :: [Maybe a] }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, Semigroup, Monoid
#ifdef MK_HASHABLE
    , Hashable )
#else
    )
#endif
#ifdef MK_ARBITRARY
  deriving newtype Arbitrary
#endif
#ifdef MK_FLAT
  deriving newtype Flat
#endif

instance (FromField a, Typeable a) => FromField (PgTag s (PgArr a)) where
  fromField = (fmap (coerce @(PGArray (Maybe a))) .) . fromField

instance (ToField a, ToStar s) => ToField (PgTag (s :: Maybe NameNSK) (PgArr a)) where
  toField (PgTag (PgArr xs)) =
    case toField (PGArray xs) of
      Plain b -> Plain (b <> typ)
      Many zs -> Many (zs <> [Plain typ])
      x -> x
    where
      typ = encodeUtf8Builder $ maybe mempty (("::" <>) . (<> "[]") . qualName) $ demote @s

instance ToJSON a => ToJSON (PgTag (s :: Maybe NameNSK) (PgArr a)) where
  toJSON = toJSON . unPgTag

instance FromJSON a => FromJSON (PgTag (s :: Maybe NameNSK) (PgArr a)) where
  parseJSON = fmap PgTag . parseJSON

-- | Make 'PgArr' from list. All elements are lifted to 'Maybe'
pgArr' :: [a] -> PgArr a
pgArr' = PgArr . fmap Just

-- | Make list from 'PgArr'. All empty ('Nothing') elements are omitted.
unPgArr' :: PgArr a -> [a]
unPgArr' = catMaybes . unPgArr

-- | 'Oid' but with JSON instances
newtype PgOid = PgOid { fromPgOid :: Oid }
  deriving stock (Show, Read)
  deriving newtype (FromField, ToField)

instance Eq PgOid where _ == _ = True
  -- we don't want to distinguish by OIDs but by names instead
  -- e.g. if we recreate some table or constraint

instance FromJSON PgOid where
  parseJSON = fmap (PgOid . read . ("Oid " ++)) . parseJSON

instance ToJSON PgOid where
  toJSON = toJSON . L.drop 4 . P.show . fromPgOid

-- | Closed type family to check that field can be converted to or from Haskell type.
-- To make your types convertible to some database type use open type family 'CanConvert1'.
type family CanConvert sch (tab :: NameNSK) (fld::Symbol) (fd :: FldDefK) (t :: Type) :: Constraint where
  CanConvert sch tab fld fd t = CanConvertMaybe sch tab fld (FdType fd)
    (FdNullable fd) (TTypDef sch (FdType fd)) t

type ErrDesc tab fld tn t =
  ( TL.Text ""
  :$$: TL.Text "Table: " :<>: TL.ShowType tab
  :$$: TL.Text "DB Field name: " :<>: TL.ShowType fld
  :$$: TL.Text "DB Field type: " :<>: TL.ShowType tn
  :$$: TL.Text "Haskell Field type: " :<>: TL.ShowType t
  :$$: TL.Text "")

type ErrWithHead :: forall k. Symbol -> NameNSK -> Symbol -> NameNSK -> Type -> k
type ErrWithHead s tab fld tn t = TL.TypeError (TL.Text s :$$: ErrDesc tab fld tn t)

type ErrWithHead' :: forall k. ErrorMessage -> NameNSK -> Symbol -> NameNSK -> Type -> k
type ErrWithHead' msg tab fld tn t = TL.TypeError (msg :$$: ErrDesc tab fld tn t)

type family GuardConvert (ok :: Bool) sch tab fld tn td t t' err :: Constraint where
  GuardConvert 'True  sch tab fld tn td t t' err = CanConvert1 sch tab fld tn td t'
  GuardConvert 'False sch tab fld tn td t t' err = ErrWithHead err tab fld tn t

type family CanConvertMaybe sch (tab::NameNSK) (fld::Symbol) (tn::NameNSK)
  (nullable :: Bool) (td :: TypDefK) (t :: Type) :: Constraint where
  CanConvertMaybe sch tab fld tn nullable td (Maybe (Aggr fn t)) =
    ErrWithHead "You can't use Maybe for aggregates. Use Maybe inside Aggr"
      tab fld tn (Aggr fn t)
  CanConvertMaybe sch tab fld tn nullable td (Maybe (Aggr' fn t)) =
    ErrWithHead "You can't use Maybe for aggregates. Use Maybe inside Aggr'"
      tab fld tn (Aggr' fn t)
  CanConvertMaybe sch tab fld tn nullable td (Maybe t) = GuardConvert
    nullable sch tab fld tn td (Maybe t) t "You can't use Maybe for mandatory fields"
  -- aggregates --
  -- Shapes and SQL result types of aggregates are checked by 'CheckAggr'.
  -- Here only values of min/max are converted (they have the type of the field).
  CanConvertMaybe sch tab fld tn nullable td (Aggr fn t) =
    CanConvertAggr sch tab fld fn tn td (UnMaybe t)
  CanConvertMaybe sch tab fld tn nullable td (Aggr' fn t) =
    CanConvertAggr sch tab fld fn tn td (UnMaybe t)
  -- end aggregates --
  CanConvertMaybe sch tab fld tn nullable td t = GuardConvert (Not nullable)
    sch tab fld tn td t t "You have to use Maybe for nullable fields"

type family CanConvertAggr sch (tab::NameNSK) (fld::Symbol) (fn :: AggrFun)
  (tn::NameNSK) (td::TypDefK) (t :: Type) :: Constraint where
  CanConvertAggr sch tab fld 'AMin tn td t =
    If (IsMinMaxCategory td) (CanConvert1 sch tab fld tn td t) (() :: Constraint)
  CanConvertAggr sch tab fld 'AMax tn td t =
    If (IsMinMaxCategory td) (CanConvert1 sch tab fld tn td t) (() :: Constraint)
  CanConvertAggr sch tab fld fn tn td t = ()

-- | Check the Haskell type @at@ of an aggregate field (e.g. @Aggr ASum (Maybe Int64)@)
-- over the DB field @fld@ against the SQL result type of the aggregate function
-- (see 'AggrResType'). Reduces to ''True' or to a 'TypeError'.
--
-- * 'Aggr' (except @count@) requires 'Maybe': the result is NULL on an empty set.
-- * 'Aggr'' (with GROUP BY) requires 'Maybe' exactly for nullable fields.
-- * @count@ is 'Int64'.
-- * @sum@ / @avg@ results: @int8@ - 'Int64', 'Scientific' or 'Rational';
--   @numeric@ - 'Scientific' or 'Rational'; @float4@ / @float8@ - 'Double',
--   'Scientific' or 'Rational'.
-- * @min@ / @max@ have the type of the field and are converted like the field
--   ('CanConvert1').
type family CheckAggr sch (tab::NameNSK) (fld::Symbol) (fd :: FldDefK) (at :: Type)
  :: Bool where
  CheckAggr sch tab fld fd at =
    CheckAggr' tab fld (FdType fd) (FdNullable fd) (TTypDef sch (FdType fd)) at

type family CheckAggr' (tab::NameNSK) (fld::Symbol) (tn::NameNSK) (nullable :: Bool)
  (td :: TypDefK) (at :: Type) :: Bool where
  CheckAggr' tab fld tn nullable td (Aggr 'ACount Int64) = 'True
  CheckAggr' tab fld tn nullable td (Aggr 'ACount t) =
    ErrWithHead "You have to use Int64 for Aggr ACount fields" tab fld tn (Aggr 'ACount t)
  CheckAggr' tab fld tn nullable td (Aggr' 'ACount Int64) = 'True
  CheckAggr' tab fld tn nullable td (Aggr' 'ACount t) =
    ErrWithHead "You have to use Int64 for Aggr' ACount fields" tab fld tn (Aggr' 'ACount t)
  CheckAggr' tab fld tn nullable td (Aggr fn (Maybe t)) =
    CheckAggrRes tab fld fn tn (AggrResType fn tn td) (Aggr fn (Maybe t)) t
  CheckAggr' tab fld tn nullable td (Aggr fn t) = ErrWithHead
    "'Aggr' (except 'Aggr ACount') requires 'Maybe': the aggregate is NULL on an empty set. Use 'Aggr'' with GROUP BY to avoid 'Maybe' for mandatory fields"
    tab fld tn (Aggr fn t)
  CheckAggr' tab fld tn nullable td (Aggr' fn (Maybe t)) = If nullable
    (CheckAggrRes tab fld fn tn (AggrResType fn tn td) (Aggr' fn (Maybe t)) t)
    (ErrWithHead "You can't use Maybe in 'Aggr'' for mandatory fields"
      tab fld tn (Aggr' fn (Maybe t)))
  CheckAggr' tab fld tn nullable td (Aggr' fn t) = If nullable
    (ErrWithHead "You have to use Maybe in 'Aggr'' for nullable fields"
      tab fld tn (Aggr' fn t))
    (CheckAggrRes tab fld fn tn (AggrResType fn tn td) (Aggr' fn t) t)
  CheckAggr' tab fld tn nullable td at = ErrWithHead
    "Aggregate field must have type 'Aggr fn t' or 'Aggr' fn t'" tab fld tn at

-- | SQL result type of aggregate function @fn@ over a field of type @tn@
-- (with type definition @td@).
-- 'Nothing' if the aggregate is not supported for the field type.
--
-- * @count@ -> @int8@
-- * @min@ / @max@ -> type of the field (numeric, text, bool or date fields)
-- * @sum(int2 | int4)@ -> @int8@, @sum(int8 | numeric)@ -> @numeric@,
--   @sum(float4)@ -> @float4@, @sum(float8)@ -> @float8@
-- * @avg(int2 | int4 | int8 | numeric)@ -> @numeric@, @avg(float4 | float8)@ -> @float8@
type family AggrResType (fn :: AggrFun) (tn :: NameNSK) (td :: TypDefK)
  :: Maybe NameNSK where
  AggrResType 'ACount tn td = 'Just (PGC "int8")
  AggrResType 'AMin tn td = If (IsMinMaxCategory td) ('Just tn) 'Nothing
  AggrResType 'AMax tn td = If (IsMinMaxCategory td) ('Just tn) 'Nothing
  AggrResType 'ASum (PGC "int2") td = 'Just (PGC "int8")
  AggrResType 'ASum (PGC "int4") td = 'Just (PGC "int8")
  AggrResType 'ASum (PGC "int8") td = 'Just (PGC "numeric")
  AggrResType 'ASum (PGC "float4") td = 'Just (PGC "float4")
  AggrResType 'ASum (PGC "float8") td = 'Just (PGC "float8")
  AggrResType 'ASum (PGC "numeric") td = 'Just (PGC "numeric")
  AggrResType 'AAvg (PGC "int2") td = 'Just (PGC "numeric")
  AggrResType 'AAvg (PGC "int4") td = 'Just (PGC "numeric")
  AggrResType 'AAvg (PGC "int8") td = 'Just (PGC "numeric")
  AggrResType 'AAvg (PGC "numeric") td = 'Just (PGC "numeric")
  AggrResType 'AAvg (PGC "float4") td = 'Just (PGC "float8")
  AggrResType 'AAvg (PGC "float8") td = 'Just (PGC "float8")
  AggrResType fn tn td = 'Nothing

-- | Type categories supported by @min@ / @max@
-- (numeric, string, boolean and date/time).
type family IsMinMaxCategory (td :: TypDefK) :: Bool where
  IsMinMaxCategory td = Elem' (TypCategory td) '["N","S","B","D"]

type family CheckAggrRes (tab::NameNSK) (fld::Symbol) (fn :: AggrFun) (tn::NameNSK)
  (res :: Maybe NameNSK) (at :: Type) (t :: Type) :: Bool where
  CheckAggrRes tab fld fn tn 'Nothing at t = ErrWithHead'
    ( TL.Text "Aggregate '" :<>: TL.Text (AggrFunName fn)
      :<>: TL.Text "' is not supported for fields of type '"
      :<>: TL.Text (NnsName tn) :<>: TL.Text "'" )
    tab fld tn at
  -- min/max have the type of the field: values are checked by 'CanConvert1'
  CheckAggrRes tab fld 'AMin tn ('Just tn) at t = 'True
  CheckAggrRes tab fld 'AMax tn ('Just tn) at t = 'True
  CheckAggrRes tab fld fn tn ('Just rt) at t = If (AggrResCompatible rt t) 'True
    ( ErrWithHead'
      ( TL.Text "Aggregate '" :<>: TL.Text (AggrFunName fn)
        :<>: TL.Text "' over a field of type '" :<>: TL.Text (NnsName tn)
        :<>: TL.Text "' returns SQL type '" :<>: TL.Text (NnsName rt) :<>: TL.Text "',"
      :$$: TL.Text "but Haskell type " :<>: TL.ShowType t
        :<>: TL.Text " is not compatible with it."
      :$$: TL.Text "Use " :<>: TL.Text (AggrResHint rt) :<>: TL.Text " instead." )
      tab fld tn at )

type family AggrFunName (fn :: AggrFun) :: Symbol where
  AggrFunName 'ACount = "count"
  AggrFunName 'AMin = "min"
  AggrFunName 'AMax = "max"
  AggrFunName 'ASum = "sum"
  AggrFunName 'AAvg = "avg"

-- | Haskell types for SQL result types of @count@, @sum@ and @avg@.
type family AggrResCompatible (rt :: NameNSK) (t :: Type) :: Bool where
  AggrResCompatible (PGC "int8") Int64 = 'True
  AggrResCompatible (PGC "int8") Scientific = 'True
  AggrResCompatible (PGC "int8") Rational = 'True
  AggrResCompatible (PGC "numeric") Scientific = 'True
  AggrResCompatible (PGC "numeric") Rational = 'True
  AggrResCompatible (PGC "float4") Double = 'True
  AggrResCompatible (PGC "float4") Scientific = 'True
  AggrResCompatible (PGC "float4") Rational = 'True
  AggrResCompatible (PGC "float8") Double = 'True
  AggrResCompatible (PGC "float8") Scientific = 'True
  AggrResCompatible (PGC "float8") Rational = 'True
  AggrResCompatible rt t = 'False

type family AggrResHint (rt :: NameNSK) :: Symbol where
  AggrResHint (PGC "int8") = "Int64, Scientific or Rational"
  AggrResHint (PGC "numeric") = "Scientific or Rational"
  AggrResHint (PGC "float4") = "Double, Scientific or Rational"
  AggrResHint (PGC "float8") = "Double, Scientific or Rational"
  AggrResHint rt = "?"

-- | Open mapping from a PostgreSQL type (non-nullable) to a Haskell type.
-- Add your own equations to this family to support extra pairings.
type family CanConvert1 sch (tab::NameNSK) (fld::Symbol) (tn::NameNSK) (td::TypDefK) t :: Constraint

type instance CanConvert1 sch tab fld tn ('TypDef "A" ('Just n) y) (PgArr t) =
  CanConvert1 sch tab fld n (TTypDef sch n) t

type instance CanConvert1 sch tab fld tn ('TypDef "A" ('Just n) y) (PgTag ('Just n) (PgArr t)) =
  CanConvert1 sch tab fld n (TTypDef sch n) t

type instance CanConvert1 sch tab fld tn ('TypDef "B" x y) Bool = ()
type instance CanConvert1 sch tab fld (PGC "int2") ('TypDef "N" x y) Int16 = ()
type instance CanConvert1 sch tab fld (PGC "int4") ('TypDef "N" x y) Int32 = ()
type instance CanConvert1 sch tab fld (PGC "int8") ('TypDef "N" x y) Int64 = ()
type instance CanConvert1 sch tab fld (PGC "float4") ('TypDef "N" x y) Double = ()
type instance CanConvert1 sch tab fld (PGC "float8") ('TypDef "N" x y) Double = ()
-- type instance CanConvert1 sch tab fld (PGC "numeric") ('TypDef "N" x y) Double = ()
type instance CanConvert1 sch tab fld (PGC "oid") ('TypDef "N" x y) Int = ()
type instance CanConvert1 sch tab fld (PGC "numeric") ('TypDef "N" x y) (Fixed k) = () -- unsafe!
type instance CanConvert1 sch tab fld (PGC "int2") ('TypDef "N" x y) Scientific = ()
type instance CanConvert1 sch tab fld (PGC "int4") ('TypDef "N" x y) Scientific = ()
type instance CanConvert1 sch tab fld (PGC "int8") ('TypDef "N" x y) Scientific = ()
type instance CanConvert1 sch tab fld (PGC "float4") ('TypDef "N" x y) Scientific = ()
type instance CanConvert1 sch tab fld (PGC "float8") ('TypDef "N" x y) Scientific = ()
type instance CanConvert1 sch tab fld (PGC "numeric") ('TypDef "N" x y) Scientific = ()
type instance CanConvert1 sch tab fld (PGC "oid") ('TypDef "N" x y) PgOid = ()

#ifdef MK_TIME
type instance CanConvert1 sch tab fld (PGC "date") ('TypDef "D" x y) Day = ()
type instance CanConvert1 sch tab fld (PGC "time") ('TypDef "D" x y) TimeOfDay = ()
type instance CanConvert1 sch tab fld (PGC "timestamp") ('TypDef "D" x y) LocalTime = ()
type instance CanConvert1 sch tab fld (PGC "timestamptz") ('TypDef "D" x y) ZonedTime = ()
type instance CanConvert1 sch tab fld (PGC "timestamptz") ('TypDef "D" x y) UTCTime = ()
#endif

type instance CanConvert1 sch tab fld (PGC "char") ('TypDef "S" x y) PgChar = ()
type instance CanConvert1 sch tab fld (PGC "text") ('TypDef "S" x y) Text = ()
type instance CanConvert1 sch tab fld (PGC "varchar") ('TypDef "S" x y) Text = ()
type instance CanConvert1 sch tab fld (PGC "name") ('TypDef "S" x y) Text = ()
#if MK_CASE_INSENSITIVE
type instance CanConvert1 sch tab fld (PGC "citext") ('TypDef "S" Nothing '[]) (CI Text) = ()
type instance CanConvert1 sch tab fld ("public" ->> "citext") ('TypDef "S" Nothing '[]) (CI Text) = ()
#endif
type instance CanConvert1 sch tab fld (PGC "bytea") ('TypDef "U" x y) (Binary BS.ByteString) = ()
type instance CanConvert1 sch tab fld (PGC "bytea") ('TypDef "U" x y) (Binary BSL.ByteString) = ()
-- ^ Binary ByteString has no 'FromJSON'/'ToJSON' instances, so it can
-- be used only in the root table.
type instance CanConvert1 sch tab fld (PGC "jsonb") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "json") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
#ifdef MK_UUID
type instance CanConvert1 sch tab fld (PGC "uuid") ('TypDef "U" x y) UUID = ()
#endif

type instance CanConvert1 sch tab fld n ('TypDef "E" 'Nothing es) (PGEnum sch n)
  = ( TTypDef sch n ~ 'TypDef "E" 'Nothing es
    , FromJSON (PGEnum sch n)
    , ToJSON (PGEnum sch n) )

-- | Annotates a value with schema/type information for DML codecs.
--
-- You can describe rows in two ways:
--
-- 1. Ordinary Haskell records with a 'GHC.Generics.Generic' instance.
-- 2. Symbol-labelled rows built from '(=:)' and chained with '(:.)' from
--    @postgresql-simple@ (re-exported from "PgSchema.DML").
--
-- 'PgTag' is analogous to 'Data.Tagged.Tagged' from the @tagged@ package, but
-- carries JSON (and related) instances suited to @pg-schema@.
--
-- Both representations can be mixed in one row.
--
-- Two pieces of syntax cooperate with 'PgTag':
--
-- * '(=:)' builds a 'PgTag' value (field name + payload).
-- @RequiredTypeArguments@ lets you avoid noisy explicit type application on the
-- field name: you write "name" =: "John" instead of @"name" =: "John".
-- * '(:=)' is the type-level spelling of the same idea (see the '(:=)' type
--   synonym below).
--
newtype PgTag s t = PgTag { unPgTag :: t }
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

-- | Parse legacy @PgTag { unPgTag = … }@ (and minor spacing variants).
--
-- >>> P.show (fst (P.head (readsPgTagLegacy "PgTag { unPgTag = 9 }" :: [(PgTag "z" Int, String)])))
-- "\"z\" =: 9"
readsPgTagLegacy :: Read t => ReadS (PgTag s t)
readsPgTagLegacy r =
  [ (PgTag v, z)
  | ("PgTag", r1) <- lex r
  , ("{", r2) <- lex r1
  , ("unPgTag", r3) <- lex r2
  , ("=", r4) <- lex r3
  , (v, r5) <- reads r4
  , ("}", z) <- lex r5
  ]

-- | When @s :: 'Symbol'@ and @t@ is 'Show', render like @\"field\" =: value@:
--
-- >>> P.show ("ownerId" =: (1 :: Int))
-- "\"ownerId\" =: 1"
instance {-# OVERLAPPING #-} (KnownSymbol s, Show t) =>
  Show (PgTag (s :: Symbol) t) where
  showsPrec p (PgTag x) =
    showParen (p > 5) $
      showString (P.show (TL.symbolVal (Proxy @s) :: String))
        . showString " =: "
        . showsPrec 6 x

-- | Accept @\"field\" =: value@ or the legacy @PgTag { unPgTag = … }@ form:
--
-- >>> P.show (read "\"x\" =: 42" :: PgTag "x" Int)
-- "\"x\" =: 42"
-- >>> P.show (read "PgTag { unPgTag = 42 }" :: PgTag "x" Int)
-- "\"x\" =: 42"
instance {-# OVERLAPPING #-} (KnownSymbol s, Read t) =>
  Read (PgTag (s :: Symbol) t) where
  readsPrec p r =
    readParen (p > 5) tryEqColon r P.++ readParen (p > 11) readsPgTagLegacy r
    where
      tryEqColon s =
        [ (PgTag a, u)
        | (expected, t) <- reads s
        , expected == (TL.symbolVal (Proxy @s) :: String)
        , ("=:", u0) <- lex t
        , (a, u) <- readsPrec 6 u0
        ]

-- | If @s@ is not a 'Symbol', fall back to record-style 'Show':
--
-- >>> P.show (PgTag @Bool (42 :: Int))
-- "PgTag {unPgTag = 42}"
instance {-# OVERLAPPABLE #-} Show t => Show (PgTag s t) where
  showsPrec p (PgTag x) =
    showParen (p > 11) $
      showString "PgTag {unPgTag = " . shows x . showString "}"

-- | Uses only 'readsPgTagLegacy' (see there for a doctest).
--
-- >>> P.show (read "PgTag { unPgTag = 42 }" :: PgTag Bool Int)
-- "PgTag {unPgTag = 42}"
instance {-# OVERLAPPABLE #-} Read t => Read (PgTag s t) where
  readsPrec p = readParen (p > 11) readsPgTagLegacy

type s := t = PgTag s t
infixr 5 :=

(=:) :: forall b. forall a -> b -> a := b
(=:) _ = coerce
infixr 5 =:

-- | Attach field names to a list of plain rows at once.
--
-- Rows are ordinary values chained with '(:.)' (no 'PgTag' on each field);
-- the names are given only once, for the whole list:
--
-- > insertSch (MyAnn "projects") conn
-- >   $ named ["ownerId", "title", "status", "tags"]
-- >   [ (1 :: Int64) :. ("pg-schema" :: Text) :. Project_status_active :. pgArr' ["db" :: Text, "haskell"]
-- >   , 1 :. "tutorial" :. Project_status_draft :. pgArr' ["learning"] ]
--
-- is the same as
--
-- > insertSch (MyAnn "projects") conn
-- >   [ "ownerId" =: (1 :: Int64) :. "title" =: ("pg-schema" :: Text)
-- >     :. "status" =: Project_status_active :. "tags" =: pgArr' ["db" :: Text, "haskell"]
-- >   , "ownerId" =: 1 :. "title" =: "tutorial"
-- >     :. "status" =: Project_status_draft :. "tags" =: pgArr' ["learning"] ]
--
-- The n-th name is attached to the n-th field; the last name takes the whole
-- rest of the row. The number of names must be equal to the number of fields,
-- otherwise there is a compile-time error.
--
-- Values are just 'coerce'd, so 'named' costs nothing at runtime. The result
-- has exactly the same type as rows built with '(=:)', so it can be used
-- wherever such rows are accepted, including nested rows (a child list is just
-- another 'named' call).
--
-- >>> named ["a", "b"] [1 :. "x", 2 :. ("y" :: String)] :: ["a" := Int :. "b" := String]
-- ["a" =: 1 :. "b" =: "x","a" =: 2 :. "b" =: "y"]
named :: forall (ns :: [Symbol]) -> forall r. Coercible [r] [Named ns r] => [r] -> [Named ns r]
named _ = coerce

-- | Type of a row with field names @ns@ attached to the fields of plain row @r@
-- (see 'named').
--
-- >>> :kind! Named ["a", "b", "c"] (Int :. Bool :. Char)
-- Named ["a", "b", "c"] (Int :. Bool :. Char) :: *
-- = PgTag "a" Int :. (PgTag "b" Bool :. PgTag "c" Char)
type family Named (ns :: [Symbol]) (r :: Type) :: Type where
  Named '[] r = TE.TypeError
    ( TE.Text "named: the list of field names is empty."
    TE.:$$: TE.Text "  The row: " TE.:<>: TE.ShowType r )
  Named '[n] (a :. b) = TE.TypeError
    ( TE.Text "named: there are fewer field names than fields in the row."
    TE.:$$: TE.Text "  The last name " TE.:<>: TE.ShowType n
      TE.:<>: TE.Text " is given to more than one field: " TE.:<>: TE.ShowType (a :. b) )
  Named '[n] r = PgTag n r
  Named (n ': ns) (a :. b) = PgTag n a :. Named ns b
  Named (n ': ns) r = TE.TypeError
    ( TE.Text "named: there are more field names than fields in the row."
    TE.:$$: TE.Text "  Field " TE.:<>: TE.ShowType n TE.:<>: TE.Text " gets the last field "
      TE.:<>: TE.ShowType r
    TE.:$$: TE.Text "  Names without fields: " TE.:<>: TE.ShowType ns )

newtype UnsafeCol (flds :: [Symbol]) (expr :: Symbol) res = UnsafeCol
  { getUnsafeCol :: res }

instance (FromField res, AllFields sch tab flds, CheckExpr flds expr) =>
  FromField (PgTag '(sch, tab :: NameNSK) (UnsafeCol flds expr res)) where
  fromField = (fmap (PgTag . UnsafeCol) .) . fromField

type CheckExpr flds expr = CheckExprInternal flds (UnconsSymbol expr)

type family CheckExprInternal flds (m :: Maybe (Char, Symbol)) :: Constraint where
  CheckExprInternal '[] 'Nothing = ()
  CheckExprInternal (x ': xs) 'Nothing = TypeError (TL.Text "Count of field names in the list of fields is more than count of question marks in expression." )
  CheckExprInternal '[] ('Just '( '?', _)) = TypeError (TL.Text "Count of question marks in expression is more than count of field names in the list of fields." )
  CheckExprInternal (x ': xs) ('Just '( '?', s)) = CheckExprInternal xs (UnconsSymbol s)
  CheckExprInternal xs ('Just '( _, s)) = CheckExprInternal xs (UnconsSymbol s)

-- >>> :kind! CheckExpr '["a", "b"] "? + ?"
-- CheckExpr '["a", "b"] "? + ?" :: Constraint
-- = () :: Constraint

type family AllFields sch tab flds :: Constraint where
  AllFields sch tab '[] = ()
  AllFields sch tab (fld ': flds) = (AllFields sch tab flds, CDBFieldInfo sch tab fld)


instance (Show res, ToStar expr, ToStar flds) => Show (UnsafeCol flds expr res) where
  show (UnsafeCol res) = "UnsafeCol ["
    <> T.unpack (intercalate' "," $ ("'"<>) . (<>"'") <$> (demote @flds)) <> "] '"
    <> T.unpack (demote @expr) <> "' " <> P.show res

-- instance FromField

-- >>> P.show (UnsafeCol @["a","b"] @"? + ?" @Int 5)
-- "UnsafeCol ['a','b'] '? + ?' 5"
