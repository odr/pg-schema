module Database.PostgreSQL.DML.Insert where

import Data.Bifunctor
import Data.String
import Data.Text as T
import Database.PostgreSQL.DB
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec


-- TODO
-- 1. Check
-- 2. Insert tree
insertSch
  :: forall sch t r r'
  . (CQueryRecord PG sch t r, CQueryRecord PG sch t r', ToRow r, FromRow r')
  => Connection -> [r] -> IO [r']
insertSch conn = returning conn (insertText @sch @t @r @r')

insertText
  :: forall sch t r r'. (CQueryRecord PG sch t r, CQueryRecord PG sch t r')
  => Query
insertText = "insert into " `mappend` sn `mappend` "." `mappend` tn
  `mappend` "(" `mappend` fs `mappend` ") values (" `mappend` qs
  `mappend` ") returning " `mappend` fs'
  where
    qr = getQueryRecord @PG @sch @t @r
    qr' = getQueryRecord @PG @sch @t @r'
    (fs,qs) = bimap inter inter
      $ unzip [ (dbn,"?") | (FieldPlain _ dbn _) <- queryFields qr]
    fs' = inter [ dbn | (FieldPlain _ dbn _) <- queryFields qr']
    toQ = fromString . T.unpack
    tn = toQ $ tableName qr
    sn = toQ $ schemaName @sch
    inter = toQ . T.intercalate ","
