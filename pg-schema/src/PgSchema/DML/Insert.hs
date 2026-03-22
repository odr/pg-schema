{-# LANGUAGE KindSignatures #-}
module PgSchema.DML.Insert where

import Data.Bifunctor
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int
import PgSchema.Ann
import PgSchema.DML.Insert.Types
import PgSchema.Schema
import PgSchema.Types
import PgSchema.Utils.Internal


-- | Insert records into a table.
-- You can request any subset of columns from the inserted row via the result type.
--
-- All mandatory fields having no defaults should be present.
--
insertSch
  :: forall ann -> forall r r'. InsertReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
insertSch ann @r @r' conn = let sql = insertText ann @r @r' in
  trace' (T.unpack sql)
    $ fmap ((, sql) . fmap (unPgTag @ann @r'))
    . returning conn (fromString $ T.unpack sql)
    . fmap (PgTag @ann @r)

-- | Insert records into a table without @RETURNING@.
insertSch_ :: forall ann -> forall r. (InsertNonReturning ann r) =>
  Connection -> [r] -> IO (Int64, Text)
insertSch_ ann @r conn recs = let sql = insertText_ ann @r in do
  trace' (T.unpack sql)
    $ fmap (, sql)
    . executeMany conn (fromString $ T.unpack sql)
    . fmap (PgTag @ann @r) $ recs

-- | Construct SQL text for inserting records into a table and returning some fields.
insertText
  :: forall ann -> forall r r' s
  . (CRecInfo ann r, CRecInfo ann r', IsString s, Monoid s) => s
insertText ann @r @r'= insertText_ ann @r <> " returning " <> fs'
  where
    ri = getRecordInfo @ann @r'
    fs' = fromText $ T.intercalate "," [ fi.fieldDbName | fi <- ri.fields]

-- | Construct SQL text for inserting records into a table without @RETURNING@.
insertText_ :: forall ann -> forall r s. (IsString s, Monoid s) =>
  CRecInfo ann r => s
insertText_ ann @r = "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ri = getRecordInfo @ann @r
    (fs,qs) = bimap inter inter $ unzip [ (fi.fieldDbName,"?") | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    inter = fromText . T.intercalate ","
