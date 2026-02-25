{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Schema.Info where

import Data.Text as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.HListTag
import Database.PostgreSQL.Schema.Catalog
import Database.Types.SchList
import GHC.Generics
import GHC.Int


-- | Tables and views info
data PgClass = PgClass
  { class__namespace  :: PgTagged "nspname" Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: SchList PgAttribute
  , constraint__class :: SchList PgConstraint }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_class") PgClass

data PgClassShort = PgClassShort
  { class__namespace :: PgTagged "nspname" Text
  , relname          :: Text }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_class") PgClassShort

data PgAttribute = PgAttribute
  { attname         :: Text
  , attribute__type :: PgType
  , attnum          :: Int16
  , attnotnull      :: Bool
  , atthasdef       :: Bool }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_attribute") PgAttribute

data PgConstraint = PgConstraint
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , contype               :: PgChar
  , conkey                :: PgArr Int16 }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_constraint") PgConstraint

-- | Types info
data PgType = PgType
  { oid             :: PgOid
  , type__namespace :: PgTagged "nspname" Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: PgOid
  , enum__type      :: SchList PgEnum }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_type") PgType
--
data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_enum") PgEnum

-- | Foreign key info
data PgRelation = PgRelation
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , constraint__class     :: PgClassShort
  , constraint__fclass    :: PgClassShort
  , conkey                :: PgArr Int16
  , confkey               :: PgArr Int16 }
  deriving (Show,Eq,Generic)
instance IsoHListTag RenamerId PgCatalog (PGC "pg_constraint") PgRelation
