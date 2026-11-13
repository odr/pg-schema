{-# LANGUAGE OverloadedRecordDot #-}
module PgSchema.DML.UpsertByKey
  ( upsertByKey, upsertByKey_, upsertByKeyText, upsertByKeyText_ ) where

import Data.List as L
import Data.Maybe
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int
import PgSchema.Ann
import PgSchema.DML.Insert.Types
import PgSchema.DML.KeyedWrite
import Data.Map qualified as M
import PgSchema.Schema (qualName, tabInfoMap)
import PgSchema.Types
import PgSchema.Utils.Internal
import Prelude as P


-- | Insert rows into one table or update them if a row with the same key exists
-- (@INSERT … ON CONFLICT (key) DO UPDATE@).
--
-- @r@ must contain all mandatory columns and a full primary or unique key
-- ('UpsertByKeyReturning'). The key is the first fully covered one of: the
-- primary key, unique keys over @NOT NULL@ columns, other unique keys.
-- On conflict all other fields of @r@ are updated.
--
-- If all fields of @r@ belong to the key, there is nothing to update and
-- @ON CONFLICT DO NOTHING@ is used: then no row is returned for an existing row,
-- so the result can be shorter than the input.
--
-- Returning type @r'@ may name any plain columns of the table.
-- Returns the returned rows and the SQL text.
upsertByKey
  :: forall ann -> forall r r'. UpsertByKeyReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
upsertByKey ann @r @r' conn recs = let sql = upsertByKeyText ann @r @r' in
  trace' (T.unpack sql) do
    (, sql)
      <$> fmap (fmap (unPgTag @ann @r'))
      ( returning conn (fromString $ T.unpack sql)
      $ fmap (PgTag @ann @r) recs )

-- | Like 'upsertByKey', but without @RETURNING@.
--
-- Returns the number of inserted or updated rows and the SQL text.
upsertByKey_
  :: forall ann -> forall r. UpsertByKeyNonReturning ann r
  => Connection -> [r] -> IO (Int64, Text)
upsertByKey_ ann @r conn recs = let sql = upsertByKeyText_ ann @r in
  trace' (T.unpack sql)
    $ fmap (, sql)
    . executeMany conn (fromString $ T.unpack sql)
    . fmap (PgTag @ann @r) $ recs

-- | SQL text of 'upsertByKey' for one row, with a @?@ placeholder for each field of @r@.
upsertByKeyText
  :: forall ann -> forall r r' s
  . (UpsertByKeyReturning ann r r', IsString s, Monoid s) => s
upsertByKeyText ann @r @r' = upsertByKeyText_ ann @r <> " returning " <> fs'
  where
    ri' = getRecordInfo @ann @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

-- | SQL text of 'upsertByKey_' for one row, with a @?@ placeholder for each field of @r@.
upsertByKeyText_
  :: forall ann -> forall r s
  . (UpsertByKeyNonReturning ann r, IsString s, Monoid s) => s
upsertByKeyText_ ann @r = fromString $ T.unpack $ upsertByKeyStmt ann @r

upsertByKeyStmt :: forall ann -> forall r. UpsertByKeyNonReturning ann r => Text
upsertByKeyStmt ann @r
  | L.null plainsOthers = ins <> " on conflict do nothing"
  | otherwise = ins <> upsertOnConflict plainsOthers keyNames
  where
    ri = getRecordInfo @ann @r
    ti = tabInfoMap @(AnnSch ann) M.! ri.tabName
    flds = [ (fromText fi.fieldDbName, "?") | fi <- ri.fields ]
    srcFlds = fst <$> flds
    keyNames = fromMaybe [] $ pickKeyNames ti srcFlds
    (_, plainsOthers) = keyedUpdateSetAndKeys ti flds
    ins =
      "insert into " <> qualName ri.tabName
      <> "(" <> intercalate' ", " srcFlds <> ") values ("
      <> intercalate' ", " (snd <$> flds) <> ")"
