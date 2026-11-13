{-# LANGUAGE OverloadedRecordDot #-}
module PgSchema.DML.UpsertByKey
  ( upsertByKey, upsertByKey_, upsertByKeyText, upsertByKeyText_ ) where

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
-- If all fields of @r@ belong to the key, there is nothing to update: an
-- existing row is left as is, but it is returned too (the @ON CONFLICT@ clause is
-- a no-op @DO UPDATE@ of a key column).
--
-- A row with @NULL@ in a column of a unique key never conflicts (unless the
-- constraint is @NULLS NOT DISTINCT@), so it is always inserted. The same key must not occur twice in one call
-- (PostgreSQL can't update a row twice in one statement).
--
-- Returning type @r'@ may name any plain columns of the table.
-- Returns one returned row per input row (in the same order) and the SQL text.
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
-- If all fields of @r@ belong to the key, existing rows are left as is
-- (@ON CONFLICT (key) DO NOTHING@).
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
upsertByKeyText ann @r @r' =
  fromString (T.unpack $ upsertByKeyStmt ann @r True) <> " returning " <> fs'
  where
    ri' = getRecordInfo @ann @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

-- | SQL text of 'upsertByKey_' for one row, with a @?@ placeholder for each field of @r@.
upsertByKeyText_
  :: forall ann -> forall r s
  . (UpsertByKeyNonReturning ann r, IsString s, Monoid s) => s
upsertByKeyText_ ann @r = fromString $ T.unpack $ upsertByKeyStmt ann @r False

-- | @INSERT … ON CONFLICT …@ statement without @RETURNING@; the flag tells
-- whether @RETURNING@ will be added (see 'upsertOnConflict').
upsertByKeyStmt
  :: forall ann -> forall r. UpsertByKeyNonReturning ann r => Bool -> Text
upsertByKeyStmt ann @r withReturning =
  ins <> upsertOnConflict withReturning plainsOthers keyNames
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
