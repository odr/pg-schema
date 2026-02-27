{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}
module Database.PostgreSQL.Convert where

import Data.Aeson
import Data.ByteString as B.S
import Data.ByteString.Lazy as B.L
import Data.CaseInsensitive
import Data.Coerce
import Data.Fixed
import Data.Kind
import Data.List as L
import Data.Maybe as M
import Data.Text as T
import Data.Time
import Data.UUID.Types
import Database.PostgreSQL.Schema.Catalog (PGC)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.Schema.Def
import GHC.Int
import GHC.TypeLits as TL
import Prelude as P
import Type.Reflection
#ifdef MK_ARBITRARY
import Test.QuickCheck hiding (Fixed)
#endif
#ifdef MK_FLAT
import Flat
#endif
#ifdef MK_HASHABLE
import Data.Hashable
#endif


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
type instance CanConvert1 sch tab fld (PGC "numeric") ('TypDef "N" x y) Double = ()
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
type instance CanConvert1 sch tab fld (PGC "bytea") ('TypDef "U" x y) (Binary B.S.ByteString) = ()
type instance CanConvert1 sch tab fld (PGC "bytea") ('TypDef "U" x y) (Binary B.L.ByteString) = ()
-- ^ Binary ByteString has no instances for (FromJSON, ToJSON) so it can be
-- used only in the root table
type instance CanConvert1 sch tab fld (PGC "jsonb") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "json") ('TypDef "U" x y) a = (FromJSON a, ToJSON a)
type instance CanConvert1 sch tab fld (PGC "uuid") ('TypDef "U" x y) UUID = ()

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

instance (FromField a, Typeable a) => FromField (PgArr a) where
  fromField = (fmap (coerce @(PGArray (Maybe a))) .) . fromField

instance ToField a => ToField (PgArr a) where
  toField = toField . coerce @_ @(PGArray (Maybe a))

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
