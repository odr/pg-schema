{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module PgSchema.Types -- (PGEnum)
  where

import Control.Monad
import Control.Monad.Singletons
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.CaseInsensitive
import Data.Coerce
import Data.Fixed
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Singletons.TH
import Data.String
import Data.String.Singletons
import Data.Tagged
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Data.UUID.Types
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.TypeLits as TL
import GHC.Int
import Prelude as P
import Type.Reflection
import PgSchema.Schema.Catalog (PGC)
import PgSchema.Schema
import PgSchema.Utils.Internal hiding (fromText)
import Prelude.Singletons as SP

#ifdef MK_ARBITRARY
import Test.QuickCheck(Arbitrary(arbitrary), arbitraryBoundedEnum)
#endif
#ifdef MK_FLAT
import Flat as F
#endif
#ifdef MK_HASHABLE
import Data.Hashable
#endif


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

newtype Aggr (fname :: Symbol) t = Aggr { unAggr :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)
-- Using enumeration for `fname` leads to extra complexity in TH (schemaRec)

-- All aggregate functions except count can return NULL.
-- But if field under aggregate is mandatory they return NULL only on empty set
-- if there is no group by clause. E.g. 'select min(a) from t where false`
-- So we require Nullable for Aggr.
-- Aggr' is like Aggr but can't be used in select's without 'group by'.
-- So it is mandatory if field is mandatory.
newtype Aggr' (fname :: Symbol) t = Aggr' { unAggr' :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

promote [d|
  aggrFldDef'
    :: (Ord s, IsString s)
    => (NameNS' s -> Maybe (TypDef' s)) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDef' fTypDef fname mbFD = case fname of
    "count" -> pure $ FldDef (NameNS "pg_catalog" "int8") False False
    "avg" -> (\fd -> fd { fdType = NameNS "pg_catalog" "float8"}) <$> fdCheckCat ["N"]
    "sum" -> do
      fd <- fdCheckCat ["N"]
      let t0 = nnsName $ fdType fd
      if t0 >= "int" && t0 < "inu"
        then Just fd { fdType = NameNS "pg_catalog" "int8" }
        else Just fd { fdType = NameNS "pg_catalog" "float8" }
    "min" -> fdCheckCat ["N","S","B","D"]
    "max" -> fdCheckCat ["N","S","B","D"]
    _ -> Nothing
    where
      fdCheckCat cs = do
        fd <- mbFD
        td <- fTypDef $ fdType fd
        guard $ typCategory td `L.elem` cs
        pure fd

  aggrFldDef
    :: (Ord s, IsString s)
    => (NameNS' s -> Maybe (TypDef' s)) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDef fTypDef fname mbFD = case fname of
    "count" -> aggrFldDef' fTypDef fname mbFD
    _ -> (\fd -> fd { fdNullable = True }) <$> aggrFldDef' fTypDef fname mbFD

  aggrFldDefJ
    :: (Ord s, IsString s)
    => (NameNS' s -> TypDef' s) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDefJ f = aggrFldDef (Just . f)

  aggrFldDefJ'
    :: (Ord s, IsString s)
    => (NameNS' s -> TypDef' s) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDefJ' f = aggrFldDef' (Just . f)
  |]

-- Char has no ToField instance so make own char
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
-- | PGArray has no JSON instances. [] has JSON, but no PG.
-- This one has both.
-- All elements are Maybe because PostgreSql doesn't guarantee that all elements are present.
-- Tagged PgArr is safe converted to ToField with type information
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

instance (FromField a, Typeable a) => FromField (Tagged s (PgArr a)) where
  fromField = (fmap (coerce @(PGArray (Maybe a))) .) . fromField

instance (FromField a, Typeable a) => FromField (PgArr a) where
  fromField = (fmap (coerce @(PGArray (Maybe a))) .) . fromField

instance ToField a => ToField (PgArr a) where
  toField = toField . coerce @_ @(PGArray (Maybe a))

instance (ToField a, ToStar s) => ToField (Tagged (s :: Maybe NameNSK) (PgArr a)) where
  toField (Tagged (PgArr xs)) =
    case toField (PGArray xs) of
      Plain b -> Plain (b <> typ)
      Many zs -> Many (zs <> [Plain typ])
      x -> x
    where
      typ = encodeUtf8Builder $ maybe mempty (("::" <>) . (<> "[]") . qualName) $ demote @s

pgArr' :: [a] -> PgArr a
pgArr' = PgArr . fmap Just

unPgArr' :: PgArr a -> [a]
unPgArr' = catMaybes . unPgArr
--
newtype PgOid = PgOid { fromPgOid :: Oid }
  deriving stock (Show, Read)
  deriving newtype (FromField, ToField)

instance Eq PgOid where _ == _ = True
  -- we don't want to distinguish oids but names instead
  -- e.g. if we recreate some table or constraint

instance FromJSON PgOid where
  parseJSON = fmap (PgOid . read . ("Oid " ++)) . parseJSON

instance ToJSON PgOid where
  toJSON = toJSON . L.drop 4 . P.show . fromPgOid

type family CanConvert sch (tab :: NameNSK) (fld::Symbol) (t :: Type) :: Constraint where
  CanConvert sch tab fld t = CanConvertMaybe sch tab fld (FdType (GetFldDef sch tab fld))
    (FdNullable (GetFldDef sch tab fld)) t

type ErrDesc tab fld tn t =
  ( TL.Text ""
  :$$: TL.Text "Table: " :<>: TL.ShowType tab
  :$$: TL.Text "DB Field name: " :<>: TL.ShowType fld
  :$$: TL.Text "DB Field type: " :<>: TL.ShowType tn
  :$$: TL.Text "Haskell Field type: " :<>: TL.ShowType t
  :$$: TL.Text "")

type family CanConvertMaybe sch (tab::NameNSK) (fld::Symbol) (tn::NameNSK)
  (nullable :: Bool) (t :: Type) :: Constraint where
  CanConvertMaybe sch tab fld tn 'False (Maybe t) =
    TL.TypeError (TL.Text "You can't use Maybe for mandatory fields"
      :$$: ErrDesc tab fld tn t)
  CanConvertMaybe sch tab fld tn 'True (Maybe t) =
    CanConvert1 sch tab fld tn (TTypDef sch tn) t
  CanConvertMaybe sch tab fld tn 'False t =
    CanConvert1 sch tab fld tn (TTypDef sch tn) t
  CanConvertMaybe sch tab fld tn 'True t =
    TL.TypeError (TL.Text "You have to use Maybe for nullable fields"
      :$$: ErrDesc tab fld tn t)

-- | Many to many relation between db-type and Haskell type (not nullable)
-- Param `sch` is needed to describe complex types (e.g. arrays)
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
-- ^ Binary ByteString has no instances for (FromJSON, ToJSON) so it can be
-- used only in the root table
type instance CanConvert1 sch tab fld (PGC "jsonb") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "json") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "uuid") ('TypDef "U" x y) UUID = ()

type instance CanConvert1 sch tab fld n ('TypDef "E" 'Nothing es) (PGEnum sch n)
  = ( TTypDef sch n ~ 'TypDef "E" 'Nothing es
    , FromJSON (PGEnum sch n)
    , ToJSON (PGEnum sch n) )
