module Database.PostgreSQL.DML.Insert where

import Data.Text
import Database.PostgreSQL.DB
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec


insertSch
  :: forall sch t r r'
  . (CQueryRecord PG sch t r, CTabDef sch t, CQueryRecord PG sch t r')
  => Connection -> r -> IO r'
insertSch = undefined

insertText
  :: forall sch tab r. (CQueryRecord PG sch tab r, CTabDef sch tab)
  => r -> Text
insertText = undefined
