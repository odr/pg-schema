module Database.Types.EmptyField where

import Data.Aeson
import Data.Hashable
import Data.Void
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.Schema.Def
import Database.Schema.Rec
import PgSchema.Util


newtype EmptyField = EmptyField (Maybe Void) -- Maybe to omitNothingField in JSON
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON)

instance FromField EmptyField where
  fromField _ _ = pure $ EmptyField Nothing

instance ToField EmptyField where
  toField _ = toField Null

instance CRecordInfo EmptyField where
  type TRecordInfo EmptyField = '[]

instance (CSchema sch, ToStar t) => CQueryRecord db sch t EmptyField where

instance CQueryField ('FldUnknown s) db sch t '( fi, EmptyField) where
  type TQueryField ('FldUnknown s) db sch t '( fi, EmptyField) = 'QFieldEmpty s
