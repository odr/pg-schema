{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Convert where

import Data.Aeson
import Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Schema.Def
import GHC.TypeLits


-- Char has no ToField instance so make own char
newtype PgChar = PgChar { unPgChar :: Char } deriving
  (Show,Eq,Read,Ord,FromField,Enum,Bounded,FromJSON,ToJSON)

instance ToField PgChar where
  toField = toField . (:[]) . unPgChar

class (FromJSON t, ToJSON t, CTypDef sch tn)
  => CanConvert sch (tn::Symbol) (nullable :: Bool) t

class (FromJSON t, ToJSON t)
  => CanConvert1 (td::TypDef Symbol) sch (tn::Symbol) t

instance
  (CanConvert1 (TTypDef sch tn) sch tn t, CTypDef sch tn, FromJSON t, ToJSON t)
  => CanConvert sch tn 'False t

-- It is possible to do better
-- but there are too much complexity without clear profit
instance CanConvert sch tn 'False t => CanConvert sch tn 'True (Maybe t)

instance CanConvert1 ('TypDefC "B" x y) sch tn Bool

instance CanConvert sch n b t => CanConvert1 ('TypDefC "A" ('Just n) y) sch x [t]

instance CanConvert1 ('TypDefC "N" x y) sch "int2" Int
instance CanConvert1 ('TypDefC "N" x y) sch "int4" Int
instance CanConvert1 ('TypDefC "N" x y) sch "int8" Integer
instance CanConvert1 ('TypDefC "N" x y) sch "float4" Double
instance CanConvert1 ('TypDefC "N" x y) sch "float8" Double
instance CanConvert1 ('TypDefC "N" x y) sch "oid" Int
instance CanConvert1 ('TypDefC "N" x y) sch "numeric" Integer

instance CanConvert1 ('TypDefC "D" x y) sch "date" Day
instance CanConvert1 ('TypDefC "D" x y) sch "time" TimeOfDay
instance CanConvert1 ('TypDefC "D" x y) sch "timestamp" UTCTime
instance CanConvert1 ('TypDefC "D" x y) sch "timestamptz" ZonedTime

instance CanConvert1 ('TypDefC "S" x y) sch "char" PgChar
instance CanConvert1 ('TypDefC "S" x y) sch "name" Text
instance CanConvert1 ('TypDefC "S" x y) sch "text" Text
instance CanConvert1 ('TypDefC "S" x y) sch "varchar" Text
