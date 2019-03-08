module Database.PostgreSQL.DML.Insert where

import Data.Bifunctor
import Data.String
import Data.Text as T
import Database.PostgreSQL.DB
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec
import GHC.Int


-- TODO: Insert tree
insertSch
  :: forall sch t r r'
  . ( AllMandatory sch t r, CQueryRecord PG sch t r
    , CQueryRecord PG sch t r', ToRow r, FromRow r' )
  => Connection -> [r] -> IO [r']
insertSch conn = returning conn (insertText @sch @t @r @r')

insertSch_
  :: forall sch t r
  . (AllMandatory sch t r, CQueryRecord PG sch t r, ToRow r)
  => Connection -> [r] -> IO Int64
insertSch_ conn = executeMany conn (insertText_ @sch @t @r)

insertText
  :: forall sch t r r'
  . (AllMandatory sch t r, CQueryRecord PG sch t r, CQueryRecord PG sch t r')
  => Query
insertText = insertText_ @sch @t @r `mappend` " returning " `mappend` fs'
  where
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromString $ T.unpack
      $ T.intercalate "," [ dbn | (FieldPlain _ dbn _) <- queryFields qr']

insertText_
  :: forall sch t r. CQueryRecord PG sch t r => Query
insertText_ = "insert into " `mappend` sn `mappend` "." `mappend` tn
  `mappend` "(" `mappend` fs `mappend` ") values (" `mappend` qs
  `mappend` ")"
  where
    qr = getQueryRecord @PG @sch @t @r
    (fs,qs) = bimap inter inter
      $ unzip [ (dbn,"?") | (FieldPlain _ dbn _) <- queryFields qr]
    toQ = fromString . T.unpack
    tn = toQ $ tableName qr
    sn = toQ $ schemaName @sch
    inter = toQ . T.intercalate ","
