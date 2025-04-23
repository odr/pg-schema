module Database.PostgreSQL.DML.Insert where

import Data.Bifunctor
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DML.Insert.Types
import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util


insertSch :: forall sch t -> forall r r'. (InsertReturning' sch t r r') =>
  Connection -> [r] -> IO ([r'], Text)
insertSch sch t @r @r' conn = let sql = insertText sch t r r' in
  fmap (, sql) . returning conn (fromString $ T.unpack sql)

insertSch_ :: forall sch t -> forall r. (InsertNonReturning' sch t r) =>
  Connection -> [r] -> IO (Int64, Text)
insertSch_ sch t @r conn = let sql = insertText_ sch t r in
  fmap (, sql) . executeMany conn (fromString $ T.unpack sql)

insertText :: forall sch t r r' -> InsertReturning' sch t r r' => Text
insertText sch t r r' = insertText_ sch t r <> " returning " <> fs'
  where
    ri = getRecordInfo @sch @t @r'
    fs' = fromText $ T.intercalate "," [ fi.fieldDbName | fi <- ri.fields]

insertText_ :: forall sch t r -> CRecordInfo sch t r => Text
insertText_ sch t r = "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ri = getRecordInfo @sch @t @r
    (fs,qs) = bimap inter inter $ unzip [ (fi.fieldDbName,"?") | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    inter = fromText . T.intercalate ","
