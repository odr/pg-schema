module Database.Types.EmptyField where

import Data.Aeson
import Data.Hashable
import Data.Void
import Database.PostgreSQL.Simple.FromField as PG
import Database.Schema.Def
import Database.Schema.Rec
import PgSchema.Util


newtype EmptyField = EmptyField (Maybe Void) -- Maybe to omitNothingField in JSON
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON)

instance FromField EmptyField where
  fromField _ _ = pure $ EmptyField Nothing

instance CRecordInfo EmptyField where
  type TRecordInfo EmptyField = '[]

-- instance CFieldType r n => CFieldType (SchList r) n where
--   type TFieldType (SchList r) n = TFieldType r n

instance (CSchema sch, ToStar t) => CQueryRecord db sch t EmptyField where
