{-# LANGUAGE CPP #-}
module Database.Types.EmptyField where

import Data.Aeson
import Data.Hashable
import Data.Void
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat as F
#endif


newtype EmptyField = EmptyField (Maybe Void) -- Maybe to omitNothingField in JSON
  deriving newtype (Show, Eq, Ord, Hashable, FromJSON, ToJSON)

emptyField :: EmptyField
emptyField = EmptyField Nothing

#ifdef MK_FLAT
instance Flat EmptyField where
  encode _ = F.encode ()
  decode = emptyField <$ F.decode @()
  size _ = F.size ()
#endif

instance FromField EmptyField where
  fromField _ _ = pure $ EmptyField Nothing

instance ToField EmptyField where
  toField _ = toField Null

#ifdef MK_ARBITRARY
instance Arbitrary EmptyField where
  arbitrary = pure $ EmptyField Nothing
#endif
