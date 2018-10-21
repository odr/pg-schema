{-# LANGUAGE DuplicateRecordFields #-}
module Database.PostgreSQL.Schema.Info where

import Data.Aeson.TH
import Data.List as L
import Data.Text as T
import Data.UUID
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DB
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Schema.Catalog
-- import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple.Types
import Database.Schema.Rec
import Database.Schema.TH
import GHC.Generics
-- import TH.Rec.Update


data PgSchema = PgSchema
-- ^ Schema data. Three selects are needed to get it. One for each field
  { types     :: [PgType]
  , classes   :: [PgClass] -- ^ tables and views
  , relations :: [PgRelations] }
  deriving (Show,Generic)

data PgClass = PgClass
-- ^ Tables and views info
  { class__namespace  :: PgTagged "nspname" Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: [PgAttribute]
  , constraint__class :: [PgConstraint] }
  deriving (Show,Generic)

data PgAttribute = PgAttribute
  { attname         :: Text
  , attribute__type :: PgTagged "typname" Text
  , attnum          :: Int
  , attnotnull      :: Bool
  , atthasdef       :: Bool }
  deriving (Show,Generic)

data PgConstraint = PgConstraint
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , contype               :: PgChar
  , conkey                :: [Int] }
  deriving (Show,Generic)

data PgType = PgType
-- ^ Types info
  { type__namespace :: PgTagged "nspname" Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: UUID
  , enum__type      :: [PgEnum]}
  deriving (Show,Generic)

data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving (Show,Generic)

data PgRelations = PgRelations
-- ^ Foreighn key info
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , constraint__class     :: PgTagged "relname" Text
  , constraint__fclass    :: PgTagged "relname" Text
  , conkey                :: [Int]
  , confkey               :: [Int] }
  deriving (Show,Generic)

L.concat <$> mapM (deriveJSON defaultOptions)
  [ ''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelations]

-- L.concat <$> mapM mkSetField
--   [ ''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelations]

L.concat <$> mapM (schemaRec @PgCatalog id)
  [ ''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelations]

instance CQueryRecord PG PgCatalog "pg_enum" PgEnum
instance CQueryRecord PG PgCatalog "pg_constraint" PgConstraint
instance CQueryRecord PG PgCatalog "pg_attribute" PgAttribute
instance CQueryRecord PG PgCatalog "pg_class" PgClass
instance CQueryRecord PG PgCatalog "pg_type" PgType
instance CQueryRecord PG PgCatalog "pg_constraint" PgRelations

-- instance FromRow PgType
-- instance FromRow PgClass
-- instance FromRow PgRelations
