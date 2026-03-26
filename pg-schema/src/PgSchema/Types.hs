{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module PgSchema.Types(
  -- * PgTag types
  type (:=), (=:), (:.)(..), PgTag(..)
  -- * Enum
  , PGEnum
  -- * Aggregates
  , Aggr(..), Aggr'(..)
  -- * Arrays
  , PgArr(..), pgArr', unPgArr'
  -- * Other types
  , PgChar(..), PgOid(..)
  -- * Conversion checks
  , CanConvert, CanConvert1
  -- * UnsafeCol
  , UnsafeCol(..) )
  where

import Control.Monad
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.CaseInsensitive
import Data.Coerce
import Data.Fixed
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Scientific
import Data.Singletons.TH
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Data.UUID.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.TypeLits as TL
import GHC.TypeError as TE
import Data.Type.Equality (type (==))
import GHC.Int
import Prelude as P
import Type.Reflection
import PgSchema.Schema.Catalog (PGC)
import Data.Type.Bool (Not, type (&&), type (||))
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


-- | Introduce `enum` database types.
-- Data instances are produced by schema generation.
-- You can use these data instances in you records to @SELECT@/@INSERT@/@UPSERT@ data
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

-- | Introduce aggregate functions.
--
-- I.e. @"fld" := Aggr AMin (Maybe Int32)@ means "minimum value of the field `fld`"
--
-- 'Aggr' requires a 'Maybe' argument for all functions except @count@.
--
-- Only a small set of aggregations are supported currently: @count@, @min@, @max@, @sum@, @avg@.
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

type ErrWithHead s tab fld tn t = TL.TypeError (TL.Text s :$$: ErrDesc tab fld tn t)

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
  -- ACount<=>int64; AMin/AMax<=>N,S,B,D; ASum<=>N+int*->Int64|Double; AAvg<=>N+float8
  CanConvertMaybe sch tab fld tn nullable td (Aggr ACount Int64) = ()
  CanConvertMaybe sch tab fld tn nullable td (Aggr ACount t) =
    ErrWithHead "You have to use Int64 for Aggr ACount fields" tab fld tn t
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ACount Int64) = ()
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ACount t) =
    ErrWithHead "You have to use Int64 for Aggr' ACount fields" tab fld tn t
  CanConvertMaybe sch tab fld tn nullable td (Aggr AMin t) = GuardConvert
    (IsMaybe t && Elem' (TypCategory td) '["N","S","B","D"])
    sch tab fld tn td (Aggr AMin t) (UnMaybe t)
    "'Aggr AMin' is possible only for 'Maybe' values and numeric, text, bool or date fields"
  CanConvertMaybe sch tab fld tn nullable td (Aggr' AMin t) = GuardConvert
    (Elem' (TypCategory td) '["N","S","B","D"]) sch tab fld tn td (Aggr' AMin t) t
      "'Aggr' AMin' is possible only for numeric, text, bool or date fields"
  CanConvertMaybe sch tab fld tn nullable td (Aggr AMax t) = GuardConvert
    (IsMaybe t && Elem' (TypCategory td) '["N","S","B","D"])
    sch tab fld tn td (Aggr AMax t) (UnMaybe t)
    "'Aggr AMax' is possible only for 'Maybe' values and numeric, text, bool or date fields"
  CanConvertMaybe sch tab fld tn nullable td (Aggr' AMax t) = GuardConvert
    (Elem' (TypCategory td) '["N","S","B","D"])
    sch tab fld tn td (Aggr' AMax t) t
    "'Aggr' AMax' is possible only for numeric, text, bool or date fields"
  CanConvertMaybe sch tab fld tn nullable td (Aggr AAvg (Maybe Scientific)) =
    Assert (TypCategory td == "N") (ErrWithHead
      "'Aggr AAvg' is possible only for numeric fields"
      tab fld tn (Aggr AAvg (Maybe Scientific)))
  CanConvertMaybe sch tab fld tn nullable td (Aggr AAvg t) = ErrWithHead
    "You have to use `Maybe Scientific` for `Aggr AAvg` fields"
    tab fld tn (Aggr AAvg t)
  CanConvertMaybe sch tab fld tn nullable td (Aggr' AAvg (Maybe Scientific)) =
    Assert (TypCategory td == "N" && nullable) (ErrWithHead
      "'Aggr' AAvg (Maybe Scientific)' is possible only for nullable numeric fields"
      tab fld tn (Aggr' AAvg (Maybe Scientific)))
  CanConvertMaybe sch tab fld tn nullable td (Aggr' AAvg Scientific) =
    Assert (TypCategory td == "N" && Not nullable) (ErrWithHead
      "'Aggr' AAvg Scientific' is possible only for mandatory numeric fields"
      tab fld tn (Aggr' AAvg Scientific))
  CanConvertMaybe sch tab fld tn nullable td (Aggr' AAvg t) = ErrWithHead
    "You have to use `Scientific` (or `Maybe Scientific`) for `Aggr' AAvg` fields"
    tab fld tn (Aggr' AAvg t)
  CanConvertMaybe sch tab fld tn nullable td (Aggr ASum (Maybe Int64)) =
    ( Assert (TypCategory td == "N" && (NnsName tn == "int2" || NnsName tn == "int4"))
      (ErrWithHead "'Aggr ASum (Maybe Int64)' is possible only for 'int2/int4' fields"
        tab fld tn (Aggr ASum (Maybe Int64))))
  CanConvertMaybe sch tab fld tn nullable td (Aggr ASum (Maybe Scientific)) =
    ( Assert (TypCategory td == "N")
      (ErrWithHead "'Aggr ASum (Maybe Scientific)' is possible only for numeric fields"
        tab fld tn (Aggr ASum (Maybe Scientific))))
  CanConvertMaybe sch tab fld tn nullable td (Aggr ASum t) = ErrWithHead
    "You have to use `Maybe Int64` (or `Maybe Scientific`) for `Aggr ASum` fields"
    tab fld tn (Aggr ASum t)
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ASum (Maybe Int64)) =
    ( Assert (nullable && TypCategory td == "N" && (NnsName tn == "int2" || NnsName tn == "int4"))
      (ErrWithHead "'Aggr' ASum (Maybe Int64)' is possible only for nullable 'int2/int4' fields"
        tab fld tn (Aggr' ASum (Maybe Int64))))
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ASum (Maybe Scientific)) =
    ( Assert (nullable && TypCategory td == "N")
      (ErrWithHead "'Aggr' ASum (Maybe Scientific)' is possible only for nullable numeric fields"
        tab fld tn (Aggr' ASum (Maybe Scientific))))
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ASum Int64) =
    ( Assert (Not nullable && TypCategory td == "N" && (NnsName tn == "int2" || NnsName tn == "int4"))
      (ErrWithHead "'Aggr' ASum Int64' is possible only for mandatory 'int2/int4' fields"
        tab fld tn (Aggr' ASum Int64)))
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ASum Scientific) =
    ( Assert (Not nullable && TypCategory td == "N")
      (ErrWithHead "'Aggr' ASum (Scientific)' is possible only for mandatory numeric fields"
        tab fld tn (Aggr' ASum Scientific)))
  CanConvertMaybe sch tab fld tn nullable td (Aggr' ASum t) = ErrWithHead
    "You have to use `[Maybe] Int64` (or `[Maybe] Scientific`) for `Aggr' ASum` fields"
    tab fld tn (Aggr' ASum t)
  -- end aggregates --
  CanConvertMaybe sch tab fld tn nullable td t = GuardConvert (Not nullable)
    sch tab fld tn td t t "You have to use Maybe for nullable fields"

-- | Open mapping from a PostgreSQL type (non-nullable) to a Haskell type.
-- Add your own equations to this family to support extra pairings.
type family CanConvert1 sch (tab::NameNSK) (fld::Symbol) (tn::NameNSK) (td::TypDefK) t :: Constraint

type instance CanConvert1 sch tab fld tn ('TypDef "A" ('Just n) y) (PgArr t) =
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

type instance CanConvert1 sch tab fld (PGC "date") ('TypDef "D" x y) Day = ()
type instance CanConvert1 sch tab fld (PGC "time") ('TypDef "D" x y) TimeOfDay = ()
type instance CanConvert1 sch tab fld (PGC "timestamp") ('TypDef "D" x y) LocalTime = ()
type instance CanConvert1 sch tab fld (PGC "timestamptz") ('TypDef "D" x y) ZonedTime = ()
type instance CanConvert1 sch tab fld (PGC "timestamptz") ('TypDef "D" x y) UTCTime = ()

type instance CanConvert1 sch tab fld (PGC "char") ('TypDef "S" x y) PgChar = ()
type instance CanConvert1 sch tab fld (PGC "text") ('TypDef "S" x y) Text = ()
type instance CanConvert1 sch tab fld (PGC "varchar") ('TypDef "S" x y) Text = ()
type instance CanConvert1 sch tab fld (PGC "name") ('TypDef "S" x y) Text = ()
type instance CanConvert1 sch tab fld (PGC "citext") ('TypDef "S" Nothing '[]) (CI Text) = ()
type instance CanConvert1 sch tab fld ("public" ->> "citext") ('TypDef "S" Nothing '[]) (CI Text) = ()
type instance CanConvert1 sch tab fld (PGC "bytea") ('TypDef "U" x y) (Binary BS.ByteString) = ()
type instance CanConvert1 sch tab fld (PGC "bytea") ('TypDef "U" x y) (Binary BSL.ByteString) = ()
-- ^ Binary ByteString has no 'FromJSON'/'ToJSON' instances, so it can
-- be used only in the root table.
type instance CanConvert1 sch tab fld (PGC "jsonb") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "json") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "uuid") ('TypDef "U" x y) UUID = ()

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
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

type s := t = PgTag s t
infixr 5 :=

(=:) :: forall b. forall a -> b -> a := b
(=:) _ = coerce
infixr 5 =:

newtype UnsafeCol (flds :: [Symbol]) (expr :: Symbol) res = UnsafeCol
  { getUnsafeCol :: res }
  -- deriving newtype (FromField, FromJSON)

instance (FromField res, AllFields sch tab flds, CheckExpr flds expr) =>
  FromField (PgTag '(sch, tab :: NameNSK) (UnsafeCol flds expr res)) where
  fromField = (fmap (PgTag . UnsafeCol) .) . fromField

type CheckExpr flds expr = CheckExprInternal flds (UnconsSymbol expr)

type family CheckExprInternal flds (m :: Maybe (Char, Symbol)) :: Constraint where
  CheckExprInternal '[] 'Nothing = ()
  CheckExprInternal (x ': xs) 'Nothing = TypeError (TL.Text "Count of field names in the list of fields is more then count of question marks in expression" )
  CheckExprInternal '[] ('Just '( '?', _)) = TypeError (TL.Text "Count of question marks in expression is more then count of field names in the list of fields" )
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
