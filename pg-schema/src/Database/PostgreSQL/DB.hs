module Database.PostgreSQL.DB where

import Database.PostgreSQL.Convert
import Database.Schema.Rec


data PG

instance CanConvertPG sch tn b t => CanConvert PG sch tn b t
