{-# LANGUAGE KindSignatures #-}
module PgSchema.DML.Insert where

import Data.Bifunctor
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int
import PgSchema.DML.Insert.Types
import PgSchema.Schema
import PgSchema.Utils.Internal
import PgSchema.HList

-- | Insert records into table.
-- We can get any fields from inserted record in returned result.
--
-- All mandatory fields having no defaults should be present.
--
insertSch
  :: forall ren sch t -> forall r r' h h'. InsertReturning' ren sch t r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
insertSch ren sch t @r @r' @h @h' conn = let sql = insertText sch t @h @h' in
  trace' (T.unpack sql)
    $ fmap ((, sql) . fmap (fromHList @ren @sch @t @r'))
    . returning conn (fromString $ T.unpack sql)
    . fmap (toHList @ren @sch @t @r)

-- | Insert records into table without returnings.
insertSch_ :: forall ren sch t -> forall r h. (InsertNonReturning' ren sch t r h) =>
  Connection -> [r] -> IO (Int64, Text)
insertSch_ ren sch t @r @h conn recs = let sql = insertText_ sch t @h in do
  trace' (T.unpack sql)
    $ fmap (, sql)
    . executeMany conn (fromString $ T.unpack sql)
    . fmap (toHList @ren @sch @t @r) $ recs

-- | Construct SQL text for inserting records into table and returning some fields.
insertText
  :: forall sch t -> forall r r' s
  . (CHListInfo sch t r, CHListInfo sch t r', IsString s, Monoid s) => s
insertText sch t @r @r'= insertText_ sch t @r <> " returning " <> fs'
  where
    ri = getRecordInfo @sch @t @r'
    fs' = fromText $ T.intercalate "," [ fi.fieldDbName | fi <- ri.fields]

-- | Construct SQL text for inserting records into table without returnings.
insertText_ :: forall sch t -> forall r s. (IsString s, Monoid s) =>
  CHListInfo sch t r => s
insertText_ sch t @r = "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ri = getRecordInfo @sch @t @r
    (fs,qs) = bimap inter inter $ unzip [ (fi.fieldDbName,"?") | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    inter = fromText . T.intercalate ","
