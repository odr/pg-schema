{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.PostgreSQL.Convert where

import Control.Monad.Zip
import Data.Aeson
import Data.ByteString as BS.S
import Data.ByteString.Lazy as BS.L
import Data.Coerce
import Data.Fixed
import Data.Hashable
import Data.List as L
import Data.Text as T
import Data.Time
import Database.PostgreSQL.Schema.Catalog (PGC)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.Schema.Def
import Type.Reflection


class ({-FromJSON t, ToJSON t, -}CTypDef sch tn)
  => CanConvertPG sch (tn::NameNSK) (nullable :: Bool) t

class {-(FromJSON t, ToJSON t)
  => -}CanConvert1 (td::TypDefK) sch (tn::NameNSK) t

instance
  (CanConvert1 (TTypDef sch tn) sch tn t, CTypDef sch tn{-, FromJSON t, ToJSON t -})
  => CanConvertPG sch tn 'False t

-- Char has no ToField instance so make own char
newtype PgChar = PgChar { unPgChar :: Char }
  deriving (Show, Eq, Read, Ord, FromField, Enum, Bounded, FromJSON, ToJSON
    , Hashable)

instance ToField PgChar where
  toField = toField . (:[]) . unPgChar

newtype PgArr a = PgArr { getPgArr :: [a] }
  -- ^ PGArray has no JSON instances. [] has JSON, but no PG.
  -- This one has both.
  deriving (Show, Eq, Ord, Read, FromJSON, ToJSON, Functor, Applicative, Monad
    , MonadZip, Foldable, Hashable)

instance (FromField a, Typeable a) => FromField (PgArr a) where
  fromField = (fmap (coerce @(PGArray a)) .) . fromField

instance ToField a => ToField (PgArr a) where
  toField = toField . coerce @_ @(PGArray a)

-- It is possible to do better
-- but there are too much complexity without clear profit
instance CanConvertPG sch tn 'False t => CanConvertPG sch tn 'True (Maybe t)

instance CanConvert1 ('TypDef "B" x y) sch tn Bool

instance CanConvertPG sch n 'False t
  => CanConvert1 ('TypDef "A" ('Just n) y) sch x (PgArr t)

newtype PgOid = PgOid { fromPgOid :: Oid }
  deriving (Show, Eq, Read, Ord, FromField, ToField)

instance Hashable PgOid where
  hashWithSalt s = hashWithSalt s . show

instance FromJSON PgOid where
  parseJSON = fmap (PgOid . read . ("Oid " ++)) . parseJSON

instance ToJSON PgOid where
  toJSON = toJSON . L.drop 4 . show . fromPgOid

instance CanConvert1 ('TypDef "N" x y) sch (PGC "int2") Int
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int4") Int
instance CanConvert1 ('TypDef "N" x y) sch (PGC "int8") Integer
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

instance CanConvert1 ('TypDef "U" x y) sch (PGC "bytea") (Binary BS.S.ByteString)
instance CanConvert1 ('TypDef "U" x y) sch (PGC "bytea") (Binary BS.L.ByteString)
-- ^ Binary ByteString has no instances for (FromJSON, ToJSON) so it can be
-- used only in the root table
