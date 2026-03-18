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
-- Plain Insert / Upsert / Update (Ann-based, без HList)
--------------------------------------------------------------------------------

type PlainIn ann r = (CRecInfo ann r, AllPlain ann r, ToRow (Tagged ann r))

type PlainOut ann r' = (CRecInfo ann r', AllPlain ann r', FromRow (Tagged ann r'))

-- | Plain insert without RETURNING.
-- Check that all fields belong to the root table and all mandatory fields are present.
type InsertNonReturning ann r =
  (PlainIn ann r, CheckAllMandatory ann (ColsDbNames (Cols ann r)))

-- | Plain insert with RETURNING.
-- Check that all inserted and returned fields belong to the root table
-- and all mandatory fields are present.
type InsertReturning ann r r' = (InsertNonReturning ann r, PlainOut ann r')

-- | Plain update with RETURNING.
-- Check that all updated and returned fields belong to the root table.
type UpdateReturning ann r r' = (UpdateNonReturning ann r, PlainOut ann r')

-- | Plain update without RETURNING.
-- Check that all updated fields belong to the root table.
type UpdateNonReturning ann r = PlainIn ann r

type TreeSch ann sch ren tab = (ann ~ 'Ann ren sch tab, CSchema sch)

type TreeIn ann r = (CRecInfo ann r, ToJSON (Tagged ann r))

type TreeOut ann r' = (CRecInfo ann r', FromJSON (Tagged ann r'),
  Typeable ann, Typeable r')

-- | Insert tree without RETURNING.
--
-- Check that all mandatory fields are present in all tables in tree.
-- Reference fields in the child tables are not checked - they are inserted automatically.
type InsertTreeNonReturning ren sch tab r =
  (CSchema sch, TreeIn ('Ann ren sch tab) r, AllMandatoryTree ('Ann ren sch tab) r '[])

-- | Insert tree with RETURNING.
--
-- Check that all mandatory fields are present in all tables in tree.
-- Reference fields in the child tables are not checked - they are inserted automatically.
--
-- It also checks that we get returnings only from the tables we inserted into.
type InsertTreeReturning ren sch tab r r' =
  ( InsertTreeNonReturning ren sch tab r, TreeOut ('Ann ren sch tab) r'
  , ReturningIsSubtree ('Ann ren sch tab) r r' )

-- | Upsert tree without RETURNING.
--
-- Check that all mandatory fields or primary keys are present in all tables in tree.
-- Reference fields in the child tables are not checked - they are inserted automatically.
type UpsertTreeNonReturning ren sch tab r =
  (CSchema sch, TreeIn ('Ann ren sch tab) r, AllMandatoryOrHasPKTree ('Ann ren sch tab) r '[])

-- | Upsert tree with RETURNING.
--
-- Check that all mandatory fields or primary keys are present in all tables in tree.
-- Reference fields in the child tables are not checked - they are inserted automatically.
--
-- It also checks that we get returnings only from the tables we upserted into.
type UpsertTreeReturning ren sch tab r r' =
  ( UpsertTreeNonReturning ren sch tab r, TreeOut ('Ann ren sch tab) r'
  , ReturningIsSubtree ('Ann ren sch tab) r r' )
