{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.PostgreSQL.Convert where

import Control.Monad.Zip
import Data.Aeson
import Data.ByteString as B.S
import Data.ByteString.Lazy as B.L
import Data.Coerce
import Data.Fixed
import Data.Hashable
import Data.Kind
import Data.List as L
import Data.Text as T
import Data.Time
import Database.PostgreSQL.Schema.Catalog (PGC)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.Schema.Def
import GHC.Int
import Type.Reflection


-- | Many to many relation between (db-type, is nullable field) and Haskell type
class CTypDef sch tn => CanConvertPG sch (tn::NameNSK) (nullable :: Bool) t

-- It is possible to do better
-- but there are too much complexity without clear profit
instance CanConvertPG sch tn 'False t => CanConvertPG sch tn 'True (Maybe t)

-- | Many to many relation between db-type and Haskell type (not nullable)
-- Param `sch` is needed to describe complex types (e.g. arrays)
class CanConvert1 (td::TypDefK) sch (tn::NameNSK) t

instance
  (CTypDef sch tn, CanConvert1 (TTypDef sch tn) sch tn t)
  => CanConvertPG sch tn 'False t

instance CanConvert1 (TTypDef sch n) sch n t
  => CanConvert1 ('TypDef "A" ('Just n) y) sch x (PgArr t)

instance CanConvert1 ('TypDef "B" x y) sch tn Bool
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int2") Int16
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int4") Int32
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int8") Int64
instance CanConvert1 ('TypDef "N" x y) sch (PGC "float4") Double
instance CanConvert1 ('TypDef "N" x y) sch (PGC "float8") Double
instance CanConvert1 ('TypDef "N" x y) sch (PGC "oid") Int
instance CanConvert1 ('TypDef "N" x y) sch (PGC "numeric") Integer
instance CanConvert1 ('TypDef "N" x y) sch (PGC "numeric") Centi
instance CanConvert1 ('TypDef "N" x y) sch (PGC "oid") PgOid

instance CanConvert1 ('TypDef "D" x y) sch (PGC "date") Day
instance CanConvert1 ('TypDef "D" x y) sch (PGC "time") TimeOfDay
instance CanConvert1 ('TypDef "D" x y) sch (PGC "timestamp") UTCTime
instance CanConvert1 ('TypDef "D" x y) sch (PGC "timestamptz") ZonedTime

instance CanConvert1 ('TypDef "S" x y) sch (PGC "char") PgChar
instance CanConvert1 ('TypDef "S" x y) sch (PGC "name") Text
instance CanConvert1 ('TypDef "S" x y) sch (PGC "text") Text
instance CanConvert1 ('TypDef "S" x y) sch (PGC "varchar") Text

-- ^ Binary ByteString has no instances for (FromJSON, ToJSON) so it can be
-- used only in the root table
instance CanConvert1 ('TypDef "U" x y) sch (PGC "bytea") (Binary B.S.ByteString)
instance CanConvert1 ('TypDef "U" x y) sch (PGC "bytea") (Binary B.L.ByteString)
instance CanConvert1 ('TypDef "U" x y) sch (PGC "jsonb") Value


class CanConvertPG sch tn nullable (DefType sch tn nullable)
  => DefConvertPG sch tn nullable where
  type DefType sch tn nullable :: Type

instance DefConvertPG sch tn 'False => DefConvertPG sch tn 'True where
  type DefType sch tn 'True = Maybe (DefType sch tn 'False)

class CanConvert1 td sch tn (DefType1 td sch tn) => DefConvert1 td sch tn where
  type DefType1 td sch tn :: Type

instance
  (CTypDef sch tn, DefConvert1 (TTypDef sch tn) sch tn)
  => DefConvertPG sch tn 'False where
  type DefType sch tn 'False = DefType1 (TTypDef sch tn) sch tn

instance DefConvert1 (TTypDef sch n) sch n
  => DefConvert1 ('TypDef "A" ('Just n) y) sch x where
  type DefType1 ('TypDef "A" ('Just n) y) sch x =
    PgArr (DefType1 (TTypDef sch n) sch n)

instance DefConvert1 ('TypDef "B" x y) sch tn where
  type DefType1 ('TypDef "B" x y) sch tn = Bool
instance DefConvert1 ('TypDef "N" x y) sch (PGC "int2") where
  type DefType1 ('TypDef "N" x y) sch (PGC "int2") = Int16
instance DefConvert1 ('TypDef "N" x y) sch (PGC "int4") where
  type DefType1 ('TypDef "N" x y) sch (PGC "int4") = Int32
instance DefConvert1 ('TypDef "N" x y) sch (PGC "int8") where
  type DefType1 ('TypDef "N" x y) sch (PGC "int8") = Int64
instance DefConvert1 ('TypDef "N" x y) sch (PGC "float4") where
  type DefType1 ('TypDef "N" x y) sch (PGC "float4") = Double
instance DefConvert1 ('TypDef "N" x y) sch (PGC "float8") where
  type DefType1 ('TypDef "N" x y) sch (PGC "float8") = Double
instance DefConvert1 ('TypDef "N" x y) sch (PGC "numeric") where
  type DefType1 ('TypDef "N" x y) sch (PGC "numeric") = Integer
instance DefConvert1 ('TypDef "N" x y) sch (PGC "oid") where
  type DefType1 ('TypDef "N" x y) sch (PGC "oid") = PgOid

instance DefConvert1 ('TypDef "D" x y) sch (PGC "date") where
  type DefType1 ('TypDef "D" x y) sch (PGC "date") = Day
instance DefConvert1 ('TypDef "D" x y) sch (PGC "time") where
  type DefType1 ('TypDef "D" x y) sch (PGC "time") = TimeOfDay
instance DefConvert1 ('TypDef "D" x y) sch (PGC "timestamp") where
  type DefType1 ('TypDef "D" x y) sch (PGC "timestamp") = UTCTime
instance DefConvert1 ('TypDef "D" x y) sch (PGC "timestamptz") where
  type DefType1 ('TypDef "D" x y) sch (PGC "timestamptz") = ZonedTime
instance DefConvert1 ('TypDef "S" x y) sch (PGC "char") where
  type DefType1 ('TypDef "S" x y) sch (PGC "char") = PgChar
instance DefConvert1 ('TypDef "S" x y) sch (PGC "name") where
  type DefType1 ('TypDef "S" x y) sch (PGC "name") = Text
instance DefConvert1 ('TypDef "S" x y) sch (PGC "text") where
  type DefType1 ('TypDef "S" x y) sch (PGC "text") = Text
instance DefConvert1 ('TypDef "S" x y) sch (PGC "varchar") where
  type DefType1 ('TypDef "S" x y) sch (PGC "varchar") = Text

instance DefConvert1 ('TypDef "U" x y) sch (PGC "bytea") where
  type DefType1 ('TypDef "U" x y) sch (PGC "bytea") = (Binary B.L.ByteString)
instance DefConvert1 ('TypDef "U" x y) sch (PGC "jsonb") where
  type DefType1 ('TypDef "U" x y) sch (PGC "jsonb") = Value

-- Char has no ToField instance so make own char
newtype PgChar = PgChar { unPgChar :: Char }
  deriving (Show, Eq, Read, Ord, FromField, Enum, Bounded, FromJSON, ToJSON
    , Hashable)

instance ToField PgChar where
  toField = toField . (:[]) . unPgChar

--
newtype PgArr a = PgArr { getPgArr :: [a] }
  -- ^ PGArray has no JSON instances. [] has JSON, but no PG.
  -- This one has both.
  deriving (Show, Eq, Ord, Read, FromJSON, ToJSON, Functor, Applicative, Monad
    , MonadZip, Foldable, Hashable, Semigroup, Monoid)

instance (FromField a, Typeable a) => FromField (PgArr a) where
  fromField = (fmap (coerce @(PGArray a)) .) . fromField

instance ToField a => ToField (PgArr a) where
  toField = toField . coerce @_ @(PGArray a)

--
newtype PgOid = PgOid { fromPgOid :: Oid }
  deriving (Show, Eq, Read, Ord, FromField, ToField)

instance Hashable PgOid where
  hashWithSalt _ _ = 0
  -- we don't want to distinguish oids only real structure
  -- e.g. if we recreate some table or constraint

instance FromJSON PgOid where
  parseJSON = fmap (PgOid . read . ("Oid " ++)) . parseJSON

instance ToJSON PgOid where
  toJSON = toJSON . L.drop 4 . show . fromPgOid
