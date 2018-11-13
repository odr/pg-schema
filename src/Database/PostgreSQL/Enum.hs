{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Enum where

import Data.Aeson
import Data.Kind
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Schema.Def
import GHC.Generics
import GHC.TypeLits
import Type.Reflection
import Util.ToStar


data family PGEnum sch (name :: Symbol) :: Type

instance
  (Read (PGEnum sch name), KnownSymbol name, Typeable sch)
  => FromField (PGEnum sch name) where
  fromField f mbs =
    case parse . decodeUtf8 <$> mbs of
      Just [(x,"")] -> pure x
      _             -> returnError Incompatible f ""
    where
      parse = reads . unpack . ((toTitle (toStar @_ @name) <> "_") <>)

instance
  (Show (PGEnum sch name), ToStar name) => ToField (PGEnum sch name) where
  toField = toField . L.drop (T.length (toStar @_ @name) + 1) . show

instance
  ( TTypDef sch name ~ 'TypDef "E" 'Nothing es
  , FromJSON (PGEnum sch name)
  , ToJSON (PGEnum sch name) )
  => CanConvert1 ('TypDef "E" 'Nothing es) sch name (PGEnum sch name)

instance
  (Generic (PGEnum sch name), GFromJSON Zero (Rep (PGEnum sch name)))
  => FromJSON (PGEnum sch name)

instance
  (Generic (PGEnum sch name), GToJSON Zero (Rep (PGEnum sch name)))
  => ToJSON (PGEnum sch name)
