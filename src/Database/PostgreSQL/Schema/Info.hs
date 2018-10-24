{-# LANGUAGE DuplicateRecordFields #-}
module Database.PostgreSQL.Schema.Info where

import Control.Monad
import Data.Aeson.TH
import Data.List as L
import Data.Text as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DB
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Schema.Catalog
import Database.PostgreSQL.Simple.FromRow
import Database.Schema.Rec
import Database.Schema.TH
import GHC.Generics
import Language.Haskell.TH


data PgClass = PgClass
-- ^ Tables and views info
  { class__namespace  :: PgTagged "nspname" Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: SchList PgAttribute
  , constraint__class :: SchList PgConstraint }
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
  , conkey                :: PgArr Int }
  deriving (Show,Generic)

data PgType = PgType
-- ^ Types info
  { oid             :: PgOid
  , type__namespace :: PgTagged "nspname" Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: PgOid
  , enum__type      :: SchList PgEnum}
  deriving (Show,Generic)

data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving (Show,Generic)

data PgRelation = PgRelation
-- ^ Foreighn key info
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , constraint__class     :: PgTagged "relname" Text
  , constraint__fclass    :: PgTagged "relname" Text
  , conkey                :: PgArr Int
  , confkey               :: PgArr Int }
  deriving (Show,Generic)

L.concat
  <$> zipWithM (\n s ->
    L.concat <$> sequenceA
      [ deriveJSON defaultOptions n
      , [d|instance FromRow $(conT n)|]
      , schemaRec @PgCatalog id n
      , [d|instance CQueryRecord PG PgCatalog $(pure $ strToSym s) $(conT n)|]
      ])
  [ ''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelation]
  [ "pg_enum", "pg_type", "pg_constraint", "pg_attribute", "pg_class"
  , "pg_constraint" ]
