module Database.PostgreSQL.DML.Insert where

import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DML.Insert.Types
import Database.PostgreSQL.HListTag
import Database.Schema.Def
import Database.Schema.Rec
import PgSchema.Util


insertSch :: forall sch t ren -> forall r r' h h' .
  ( IsoHListTag ren r
  , IsoHListTag ren r'
  , fs ~ Fields ren r, h ~ HListTag fs
  , h' ~ HListTag (Fields ren r')
  , InsertReturning' sch t h h'
  , SafeInsRow sch t fs) =>
  Connection -> [r] -> IO ([r'], Text)
insertSch sch t ren @_ @_ @h @h' conn = let sql = insertText sch t @h @h' in
  trace' (T.unpack sql) . fmap ((, sql) . fmap (fromHListTag @ren))
    . returning conn (fromText sql) . fmap (toHListTag @ren)

insertSch_ :: forall sch t ren -> forall r h.
  ( IsoHListTag ren r
  , fs ~ Fields ren r, h ~ HListTag fs
  , InsertNonReturning' sch t h
  , SafeInsRow sch t fs) =>
  Connection -> [r] -> IO (Int64, Text)
insertSch_ sch t ren @_ @h conn = let sql = insertText_ sch t @h in
  trace' (T.unpack sql) . fmap (, sql)
    . executeMany conn (fromText sql) . fmap (toHListTag @ren)

insertText :: forall sch t -> forall r r'. InsertReturning' sch t r r' => Text
insertText sch t @r @r' = insertText_ sch t @r <> " returning " <> fs'
  where
    ri = getRecordInfo @sch @t @r'
    fs' = unTextI @"," $ foldMap (TextI . (.fieldDbName)) ri.fields

insertText_ :: forall sch t -> forall r. CRecordInfo sch t r => Text
insertText_ sch t @r =
  "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ri = getRecordInfo @sch @t @r
    (unTextI @"," -> fs, unTextI @"," -> qs) =
      foldMap (\fi -> (TextI fi.fieldDbName, "?")) ri.fields
    tn = fromText $ qualName ri.tabName
