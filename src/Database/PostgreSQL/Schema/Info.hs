{-# LANGUAGE DuplicateRecordFields #-}
module Database.PostgreSQL.Schema.Info where

import Data.Aeson.TH
import Data.List as L
import Data.Tagged
import Data.Text as T
import Data.UUID
import Database.PostgreSQL.Convert
import TH.Rec.Update


data PgSchema = PgSchema
-- ^ Schema data. Three selects are needed to get it. One for each field
  { types     :: [PgType]
  , classes   :: [PgClass] -- ^ tables and views
  , relations :: [PgRelations] }
  deriving Show

data PgClass = PgClass
-- ^ Tables and views info
  { class__namespace  :: Tagged "nspname" Text
  , relname           :: Text
  , relkind           :: PgChar
  , attribute__class  :: [PgAttribute]
  , constraint__class :: [PgConstraint] }
  deriving Show

data PgAttribute = PgAttribute
  { attname         :: Text
  , attribute__type :: Tagged "typname" Text
  , attnum          :: Int
  , attnotnull      :: Bool
  , atthasdef       :: Bool }
  deriving Show

data PgConstraint = PgConstraint
  { constraint__namespace :: Tagged "nspname" Text
  , conname               :: Text
  , contype               :: PgChar
  , conkey                :: [Int] }
  deriving Show

data PgType = PgType
-- ^ Types info
  { type__namespace :: Tagged "nspname" Text
  , typname         :: Text
  , typcategory     :: PgChar
  , typelem         :: UUID
  , enum__type      :: [PgEnum]}
  deriving Show

data PgEnum = PgEnum
  { enumlabel     :: Text
  , enumsortorder :: Double }
  deriving Show

data PgRelations = PgRelations
-- ^ Foreighn key info
  { constraint__namespace :: Tagged "nspname" Text
  , conname               :: Text
  , constraint__class     :: Tagged "relname" Text
  , constraint__fclass    :: Tagged "relname" Text
  , conkey                :: [Int]
  , confkey               :: [Int] }
  deriving Show

L.concat <$> mapM (deriveJSON defaultOptions)
  [''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelations]

L.concat <$> mapM mkSetField
  [''PgEnum, ''PgType, ''PgConstraint, ''PgAttribute, ''PgClass, ''PgRelations]
