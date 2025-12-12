{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.PostgreSQL.PgDistinct where

import Data.Aeson
import Data.String
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.ToRow as PG
import Database.Schema.Rec
import GHC.Generics
import Database.PostgreSQL.Convert
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat as F
#endif


newtype PgDistinct a = PgDistinct { unPgDistinct :: a }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, ToField, FromField, ToRow, IsString)
  deriving anyclass FromRow

instance CRecordInfo sch t a => CRecordInfo sch t (PgDistinct a) where
  type TRecordInfo sch t (PgDistinct a) = TRecordInfo sch t a
  getRecordInfo = (getRecordInfo @sch @t @a) { isDistinct = True }

instance {-# OVERLAPPING #-} CanConvert sch tn False a => CanConvert sch tn False (PgDistinct a)

instance {-# OVERLAPPING #-} CanConvert sch tn True a => CanConvert sch tn True (PgDistinct a)

#ifdef MK_ARBITRARY
deriving newtype instance Arbitrary a => Arbitrary (PgDistinct a)
#endif

#ifdef MK_FLAT
deriving newtype instance Flat a => Flat (PgDistinct a)
#endif
