{-# LANGUAGE UndecidableInstances #-}
module PgSchema.DML.Insert.Types where

import Data.Aeson
import Data.Typeable
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import PgSchema.Ann
import PgSchema.Types
import PgSchema.Schema

--------------------------------------------------------------------------------
-- Plain Insert / Upsert / Update
--------------------------------------------------------------------------------

-- | Check that (input) record corresponds to annotation, can be converted to Row and contains only plain fields.
type PlainIn ann r = (CRecInfo ann r, AllPlain ann r, ToRow (PgTag ann r))

-- | Check that (output) record corresponds to annotation, can be converted from Row and contains only plain fields.
type PlainOut ann r' = (CRecInfo ann r', AllPlain ann r', FromRow (PgTag ann r'))

-- | Plain insert without @RETURNING@.
-- Check that all fields belong to the root table and all mandatory fields are present.
type InsertNonReturning ann r =
  (PlainIn ann r, CheckAllMandatory ann (ColsDbNames (Cols ann r)))

-- | Plain insert with @RETURNING@.
-- Check that all inserted and returned fields belong to the root table
-- and all mandatory fields are present.
type InsertReturning ann r r' =
  ( InsertNonReturning ann r, PlainOut ann r', ReturningMatchesInsert ann r r' )

-- | Plain update with @RETURNING@.
-- Check that all updated and returned fields belong to the root table.
type UpdateReturning ann r r' = (UpdateNonReturning ann r, PlainOut ann r')

-- | Plain update without @RETURNING@.
-- Check that all updated fields belong to the root table.
type UpdateNonReturning ann r = PlainIn ann r

-- | Upsert one table row by primary / unique key (@ToRow@, no JSON).
--
-- Requires all mandatory fields and a full primary or eligible unique key so
-- the operation is always @INSERT … ON CONFLICT …@. Use 'InsertNonReturning' or
-- 'UpdateByKeyNonReturning' for insert-only or update-only flat writes.
type UpsertByKeyNonReturning ann r =
  ( PlainIn ann r, HasSchema ann
  , CheckAllMandatoryAndHasKey ann (ColsDbNames (Cols ann r)) )

type UpsertByKeyReturning ann r r' =
  ( UpsertByKeyNonReturning ann r, PlainOut ann r'
  , ReturningMatchesUpsert ann r r' )

-- | Update one table row by primary / unique key (@ToRow@, no JSON).
type UpdateByKeyNonReturning ann r =
  ( PlainIn ann r, HasSchema ann
  , CheckHasKey ann (ColsDbNames (Cols ann r)) )

-- | Flat update by key: @r'@ is a bare returning row; the API returns
-- @IO ([Maybe r'], Text)@ (@Nothing@ when no row matched the key).
type UpdateByKeyReturning ann r r' =
  ( UpdateByKeyNonReturning ann r, PlainOut ann r'
  , ReturningIsSubtree ann r r' )

-- | Check that annotation contains a schema.
type HasSchema ann = CSchema (AnnSch ann)

-- | Check that (input) record corresponds to annotation and can be converted to JSON.
type TreeIn ann r = (CRecInfo ann r, ToJSON (PgTag ann r))

-- | Check that (output) record corresponds to annotation and can be converted from JSON.
type TreeOut ann r' = (CRecInfo ann r', FromJSON (PgTag ann r'),
  Typeable ann, Typeable r')

-- | Insert tree without @RETURNING@.
--
-- Check that all mandatory fields are present in all tables in tree.
-- Reference fields in the child tables are not checked - they are inserted automatically.
type InsertTreeNonReturning ann r =
  (HasSchema ann, TreeIn ann r, AllMandatoryTree ann r '[])

-- | Insert tree with @RETURNING@.
--
-- Check that all mandatory fields are present in all tables in tree.
-- Reference fields in the child tables are not checked - they are inserted automatically.
--
-- It also checks that we get returnings only from the tables we inserted into.
type InsertTreeReturning ann r r' =
  ( InsertTreeNonReturning ann r, TreeOut ann r', ReturningMatchesInsert ann r r' )

-- | Upsert tree without @RETURNING@.
--
-- Check that all mandatory fields or a full primary / eligible unique key are
-- present at each tree node.
-- Reference fields in the child tables are not checked - they are inserted automatically.
type UpsertTreeNonReturning ann r =
  (HasSchema ann, TreeIn ann r, AllMandatoryOrHasKeyTree ann r '[])

-- | Upsert tree with @RETURNING@.
--
-- Check that all mandatory fields or a full primary / eligible unique key are
-- present at each tree node.
-- Reference fields in the child tables are not checked - they are inserted automatically.
--
-- It also checks that we get returnings only from the tables we upserted into.
type UpsertTreeReturning ann r r' =
  ( UpsertTreeNonReturning ann r, TreeOut ann r', ReturningMatchesUpsert ann r r' )

-- | Update tree without @RETURNING@ (key required at every node).
type UpdateTreeNonReturning ann r =
  (HasSchema ann, TreeIn ann r, AllHasKeyTree ann r '[])

-- | Update tree with @RETURNING@ ('Maybe' on every list row in @r'@).
type UpdateTreeReturning ann r r' =
  ( UpdateTreeNonReturning ann r, TreeOut ann r'
  , ReturningMatchesUpdate ann r r' )
