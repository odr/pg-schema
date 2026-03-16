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
type InsertNonReturning ann r =
  (PlainIn ann r, CheckAllMandatory ann (ColsDbNames (Cols ann r)))

-- | Plain insert with RETURNING.
type InsertReturning ann r r' = (InsertNonReturning ann r, PlainOut ann r')

-- | Plain update with RETURNING.
-- Для update мы не требуем покрывать mandatory/PK, только "plain" поля.
type UpdateReturning ann r r' = (UpdateNonReturning ann r, PlainOut ann r')

-- | Plain update without RETURNING.
type UpdateNonReturning ann r = PlainIn ann r

type TreeSch ann sch ren tab = (ann ~ 'Ann ren sch tab, CSchema sch)

type TreeIn ann r = (CRecInfo ann r, ToJSON (Tagged ann r))

type TreeOut ann r' = (CRecInfo ann r', FromJSON (Tagged ann r'),
  Typeable ann, Typeable r')
