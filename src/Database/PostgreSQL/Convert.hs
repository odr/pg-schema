{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.PostgreSQL.Convert where

import Data.Aeson
import Data.Text as T
import Data.Time
import Data.UUID
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Schema.Def
import GHC.TypeLits


class (FromJSON t, ToJSON t, CTypDef sch tn)
  => CanConvertPG sch (tn::Symbol) (nullable :: Bool) t

class (FromJSON t, ToJSON t)
  => CanConvert1 (td::TypDefK) sch (tn::Symbol) t

instance
  (CanConvert1 (TTypDef sch tn) sch tn t, CTypDef sch tn, FromJSON t, ToJSON t)
  => CanConvertPG sch tn 'False t

-- Char has no ToField instance so make own char
newtype PgChar = PgChar { unPgChar :: Char } deriving
  (Show, Eq, Read, Ord, FromField, Enum, Bounded, FromJSON, ToJSON)

instance ToField PgChar where
  toField = toField . (:[]) . unPgChar

-- It is possible to do better
-- but there are too much complexity without clear profit
instance CanConvertPG sch tn 'False t => CanConvertPG sch tn 'True (Maybe t)

instance CanConvert1 ('TypDef "B" x y) sch tn Bool

instance CanConvertPG sch n 'False t
  => CanConvert1 ('TypDef "A" ('Just n) y) sch x [t]

instance CanConvert1 ('TypDef "N" x y) sch "int2" Int
instance CanConvert1 ('TypDef "N" x y) sch "int4" Int
instance CanConvert1 ('TypDef "N" x y) sch "int8" Integer
instance CanConvert1 ('TypDef "N" x y) sch "float4" Double
instance CanConvert1 ('TypDef "N" x y) sch "float8" Double
instance CanConvert1 ('TypDef "N" x y) sch "oid" Int
instance CanConvert1 ('TypDef "N" x y) sch "numeric" Integer
instance CanConvert1 ('TypDef "N" x y) sch "oid" UUID

instance CanConvert1 ('TypDef "D" x y) sch "date" Day
instance CanConvert1 ('TypDef "D" x y) sch "time" TimeOfDay
instance CanConvert1 ('TypDef "D" x y) sch "timestamp" UTCTime
instance CanConvert1 ('TypDef "D" x y) sch "timestamptz" ZonedTime

instance CanConvert1 ('TypDef "S" x y) sch "char" PgChar
instance CanConvert1 ('TypDef "S" x y) sch "name" Text
instance CanConvert1 ('TypDef "S" x y) sch "text" Text
instance CanConvert1 ('TypDef "S" x y) sch "varchar" Text
