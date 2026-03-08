{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module PgSchema.PostgreSQL.Schema.Info where

import Data.Text as T
import PgSchema.PostgreSQL.Convert
import GHC.Generics
import GHC.Int
import PgSchema.Tagged


-- | Tables and views info
data PgClass = PgClass
  { class__namespace  :: "nspname" := Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: [PgAttribute]
  , constraint__class :: [PgConstraint] }
  deriving (Show,Eq,Generic)

data PgClassShort = PgClassShort
  { class__namespace :: "nspname" := Text
  , relname          :: Text }
  deriving (Show,Eq,Generic)

data PgAttribute = PgAttribute
  { attname         :: Text
  , attribute__type :: PgType
  , attnum          :: Int16
  , attnotnull      :: Bool
  , atthasdef       :: Bool }
  deriving (Show,Eq,Generic)

data PgConstraint = PgConstraint
  { constraint__namespace :: "nspname" := Text
  , conname               :: Text
  , contype               :: PgChar
  , conkey                :: PgArr Int16 }
  deriving (Show,Eq,Generic)

-- | Types info
data PgType = PgType
  { oid             :: PgOid
  , type__namespace :: "nspname" := Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: PgOid
  , enum__type      :: [PgEnum] }
  deriving (Show,Eq,Generic)

--
data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving (Show,Eq,Generic)

-- | Foreign key info
data PgRelation = PgRelation
  { constraint__namespace :: "nspname" := Text
  , conname               :: Text
  , constraint__class     :: PgClassShort
  , constraint__fclass    :: PgClassShort
  , conkey                :: PgArr Int16
  , confkey               :: PgArr Int16 }
  deriving (Show,Eq,Generic)
