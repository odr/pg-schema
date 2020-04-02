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
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.Schema.Rec
import Database.Schema.TH
import Database.Types.SchList
import GHC.Generics
import GHC.Int
import Util.TH.LiftType


-- | Tables and views info
data PgClass = PgClass
  { class__namespace  :: PgTagged "nspname" Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: SchList PgAttribute
  , constraint__class :: SchList PgConstraint }
  deriving (Show,Eq,Generic)

data PgClassShort = PgClassShort
  { class__namespace :: PgTagged "nspname" Text
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
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , contype               :: PgChar
  , conkey                :: PgArr Int16 }
  deriving (Show,Eq,Generic)

-- | Types info
data PgType = PgType
  { oid             :: PgOid
  , type__namespace :: PgTagged "nspname" Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: PgOid
  , enum__type      :: SchList PgEnum }
  deriving (Show,Eq,Generic)
--
data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving (Show,Eq,Generic)

-- | Foreign key info
data PgRelation = PgRelation
  { constraint__namespace :: PgTagged "nspname" Text
  , conname               :: Text
  , constraint__class     :: PgClassShort
  , constraint__fclass    :: PgClassShort
  , conkey                :: PgArr Int16
  , confkey               :: PgArr Int16 }
  deriving (Show,Eq,Generic)

L.concat
  <$> zipWithM (\n s ->
    L.concat <$> sequenceA
      [ deriveJSON defaultOptions n
      , [d|instance FromRow $(liftType n)|]
      , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
      , schemaRec id n
      , [d|instance CQueryRecord PG PgCatalog $(liftType s) $(liftType n)|]
      , [d|instance Hashable $(liftType n)|]
      ])
  [ ''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass
  , ''PgClassShort, ''PgRelation ]
  [ pgc "pg_enum", pgc "pg_type", pgc "pg_constraint", pgc "pg_attribute"
  , pgc "pg_class", pgc "pg_class", pgc "pg_constraint" ]
