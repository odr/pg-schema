{-# LANGUAGE DuplicateRecordFields #-}
module Database.PostgreSQL.Schema.Info where

import Control.Monad
import Data.Aeson.TH
import Data.Hashable
import Data.List as L
import Data.Text as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DB
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Schema.Catalog
import Database.PostgreSQL.Simple.FromRow
import Database.Schema.Rec
import Database.Schema.TH
import Database.Types.SchList
import GHC.Generics
import Language.Haskell.TH


-- | Tables and views info
data PgClass = PgClass
  { class__namespace  :: PgTagged "nspname" Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: SchList PgAttribute
  , constraint__class :: SchList PgConstraint }
  deriving (Show,Eq,Generic)

data PgAttribute = PgAttribute
  { attname         :: Text
  , attribute__type :: PgTagged "typname" Text
  , attnum          :: Int
  , attnotnull      :: Bool
  , atthasdef       :: Bool }
  deriving (Show,Eq,Generic)

data PgConstraint = PgConstraint
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , contype               :: PgChar
  , conkey                :: PgArr Int }
  deriving (Show,Eq,Generic)

-- | Types info
data PgType = PgType
  { oid             :: PgOid
  , type__namespace :: PgTagged "nspname" Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: PgOid
  , enum__type      :: SchList PgEnum}
  deriving (Show,Eq,Generic)

data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving (Show,Eq,Generic)

-- | Foreighn key info
data PgRelation = PgRelation
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , constraint__class     :: PgTagged "relname" Text
  , constraint__fclass    :: PgTagged "relname" Text
  , conkey                :: PgArr Int
  , confkey               :: PgArr Int }
  deriving (Show,Eq,Generic)

L.concat
  <$> zipWithM (\n s ->
    L.concat <$> sequenceA
      [ deriveJSON defaultOptions n
      , [d|instance FromRow $(conT n)|]
      , schemaRec @PgCatalog id n
      , [d|instance CQueryRecord PG PgCatalog $(pure $ strToSym s) $(conT n)|]
      , [d|instance Hashable $(conT n)|]
      ])
  [ ''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelation]
  [ "pg_enum", "pg_type", "pg_constraint", "pg_attribute", "pg_class"
  , "pg_constraint" ]
