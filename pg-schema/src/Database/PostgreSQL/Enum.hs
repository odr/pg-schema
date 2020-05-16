{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Enum(PGEnum) where

import Control.Monad
import Data.Aeson
import Data.Kind
import Data.Maybe
import Data.Singletons
import Data.Text as T
import Data.Text.Encoding as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Type.Reflection

import Database.Schema.Def
import PgSchema.Util hiding (fromText)


data family PGEnum sch (name :: NameNSK) :: Type

instance
  (Read (PGEnum sch name), ToStar name, Typeable sch, Typeable name)
  => FromField (PGEnum sch name) where
  fromField f mbs =
    case mbs >>= fromText . decodeUtf8 of
      Just x -> pure x
      _      -> returnError Incompatible f ""

instance
  (Show (PGEnum sch name), ToStar name) => ToField (PGEnum sch name) where
  toField = toField . toText

instance
  ( TTypDef sch name ~ 'TypDef "E" 'Nothing es
  , FromJSON (PGEnum sch name)
  , ToJSON (PGEnum sch name) )
  => CanConvert1 ('TypDef "E" 'Nothing es) sch name (PGEnum sch name)

instance (Read (PGEnum sch t), ToStar t) => FromJSON (PGEnum sch t) where
    parseJSON = parseJSON >=> maybe mzero pure . fromText

instance (Show (PGEnum sch t), ToStar t) => ToJSON (PGEnum sch t) where
  toJSON = toJSON . toText

fromText
  :: forall sch t. (Read (PGEnum sch t), ToStar t)
  => Text -> Maybe (PGEnum sch t)
fromText t = fmap fst . listToMaybe
  $ reads $ unpack $ ((toTitle (nnsName $ demote @t) <> "_") <>) t

toText :: forall sch t. (Show (PGEnum sch t), ToStar t) => PGEnum sch t -> Text
toText = T.drop (T.length (nnsName $ demote @t) + 1) . pack . show
