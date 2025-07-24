{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}
module Database.PostgreSQL.Convert where

import Control.Monad.Zip
import Data.Aeson
import Data.ByteString as B.S
import Data.ByteString.Lazy as B.L
import Data.CaseInsensitive
import Data.Coerce
import Data.Fixed
import Data.Hashable
import Data.List as L
import Data.Text as T
import Data.Time
import Data.UUID
import Database.PostgreSQL.Schema.Catalog (PGC)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.Schema.Def
import Database.Types.EmptyField
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


-- | Many to many relation between (db-type, is nullable field) and Haskell type
class CTypDef sch tn => CanConvert sch (tn::NameNSK) (nullable :: Bool) t

instance CanConvert sch tn 'False t => CanConvert sch tn 'True (Maybe t)

instance CTypDef sch tn => CanConvert sch tn 'True EmptyField

instance {-# OVERLAPPING #-} (CTypDef sch tn
  , TL.TypeError (TL.Text "You can't use Maybe for mandatory fields"
    :$$: TL.Text "Table: " :<>: TL.ShowType tn
    :$$: TL.Text "Field type: " :<>: TL.ShowType (Maybe t))
  )
  => CanConvert sch tn 'False (Maybe t)

-- | Many to many relation between db-type and Haskell type (not nullable)
-- Param `sch` is needed to describe complex types (e.g. arrays)
class CanConvert1 (td::TypDefK) sch (tn::NameNSK) t

instance {-# OVERLAPPABLE #-}
  (CTypDef sch tn, CanConvert1 (TTypDef sch tn) sch tn t)
  => CanConvert sch tn 'False t

instance CanConvert1 (TTypDef sch n) sch n t
  => CanConvert1 ('TypDef "A" ('Just n) y) sch x (PgArr t)

instance CanConvert1 td sch tn EmptyField

instance CanConvert1 ('TypDef "B" x y) sch tn Bool
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int2") Int16
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int4") Int32
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int8") Int64
instance CanConvert1 ('TypDef "N" x y) sch (PGC "float4") Double
instance CanConvert1 ('TypDef "N" x y) sch (PGC "float8") Double
instance CanConvert1 ('TypDef "N" x y) sch (PGC "oid") Int
instance CanConvert1 ('TypDef "N" x y) sch (PGC "numeric") (Fixed k) -- unsafe!
instance CanConvert1 ('TypDef "N" x y) sch (PGC "oid") PgOid

instance CanConvert1 ('TypDef "D" x y) sch (PGC "date") Day
instance CanConvert1 ('TypDef "D" x y) sch (PGC "time") TimeOfDay
instance CanConvert1 ('TypDef "D" x y) sch (PGC "timestamp") UTCTime
instance CanConvert1 ('TypDef "D" x y) sch (PGC "timestamptz") ZonedTime

instance CanConvert1 ('TypDef "S" x y) sch (PGC "char") PgChar
instance CanConvert1 ('TypDef "S" x y) sch (PGC "name") Text
instance CanConvert1 ('TypDef "S" x y) sch (PGC "text") Text
instance CanConvert1 ('TypDef "S" x y) sch (PGC "varchar") Text
instance CanConvert1 ('TypDef "S" Nothing '[]) sch (PGC "citext") (CI Text)
instance CanConvert1 ('TypDef "S" Nothing '[]) sch ("public" ->> "citext") (CI Text)
-- ^ Binary ByteString has no instances for (FromJSON, ToJSON) so it can be
-- used only in the root table
instance CanConvert1 ('TypDef "U" x y) sch (PGC "bytea") (Binary B.S.ByteString)
instance CanConvert1 ('TypDef "U" x y) sch (PGC "bytea") (Binary B.L.ByteString)
instance CanConvert1 ('TypDef "U" x y) sch (PGC "jsonb") Value
instance (FromJSON a, ToJSON a) => CanConvert1 ('TypDef "U" x y) sch (PGC "jsonb") a
instance CanConvert1 ('TypDef "U" x y) sch (PGC "uuid") UUID

-- Char has no ToField instance so make own char
newtype PgChar = PgChar { unPgChar :: Char }
  deriving (Show, Eq, Read, Ord, FromField, Enum, Bounded, FromJSON, ToJSON
    , Hashable)

instance ToField PgChar where
  toField = toField . (:[]) . unPgChar

--
newtype PgArr a = PgArr { unPgArr :: [a] }
  -- ^ PGArray has no JSON instances. [] has JSON, but no PG.
  -- This one has both.
  deriving stock Traversable
  deriving newtype (Show, Eq, Ord, Read, FromJSON, ToJSON, Functor
    , Applicative, Monad, MonadZip, Foldable, Hashable, Semigroup, Monoid)
#ifdef MK_ARBITRARY
  deriving newtype Arbitrary
#endif
#ifdef MK_FLAT
  deriving newtype Flat
#endif

instance (FromField a, Typeable a) => FromField (PgArr a) where
  fromField = (fmap (coerce @(PGArray a)) .) . fromField

instance ToField a => ToField (PgArr a) where
  toField = toField . coerce @_ @(PGArray a)

--
newtype PgOid = PgOid { fromPgOid :: Oid }
  deriving (Show, Read, FromField, ToField)

instance Eq PgOid where _ == _ = True
  -- we don't want to distinguish oids but names instead
  -- e.g. if we recreate some table or constraint

instance Hashable PgOid where
  hash _ = 0
  hashWithSalt _ _ = 0

instance FromJSON PgOid where
  parseJSON = fmap (PgOid . read . ("Oid " ++)) . parseJSON

instance ToJSON PgOid where
  toJSON = toJSON . L.drop 4 . P.show . fromPgOid
