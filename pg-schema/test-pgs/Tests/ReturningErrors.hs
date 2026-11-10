{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
-- | Compile-time errors of the @RETURNING@ type of 'insertJSON' / 'upsertJSON' /
-- 'updateJSON' ('ReturningMatchesInsert', 'ReturningMatchesUpsert',
-- 'ReturningMatchesUpdate').
--
-- The module is compiled with @-fdefer-type-errors@, so an unsatisfied
-- constraint becomes a thunk that throws 'TypeError' with the compile-time
-- message. Evidence of a 'Constraint' is lazy and is never used at runtime, so
-- the test builds a 'Dict' of the constraint and forces all its fields
-- ('deepForce').
module Tests.ReturningErrors where

import Control.Exception (TypeError(..), evaluate, try)
import Data.Int (Int32)
import Data.Kind (Constraint)
import Data.List qualified as L
import Data.Text (Text)
import GHC.Exts.Heap (Box(..), GenClosure(..), getClosureData)
import Hedgehog
import PgSchema.DML
import Utils

data Dict (c :: Constraint) where
  Dict :: c => Dict c

-- | Force a value and (up to some depth) all its constructor fields.
-- Dictionaries of the checked constraints are nested constraint tuples, so
-- this reaches the deferred 'TypeError' if there is one.
deepForce :: Int -> a -> IO ()
deepForce 0 _ = pure ()
deepForce n x = do
  _ <- evaluate x
  c <- getClosureData x
  case c of
    ConstrClosure{ptrArgs} -> mapM_ (\(Box y) -> deepForce (n - 1) y) ptrArgs
    _ -> pure ()

-- | Check that the constraint is not satisfied: the message of the type error
-- contains all fragments.
typeErrorWith :: [String] -> Dict c -> Property
typeErrorWith fragments d = withTests 1 $ property do
  res <- evalIO $ try @TypeError $ deepForce 100 d
  case res of
    Right _ -> do
      footnote "expected a type error, but the constraint is satisfied"
      failure
    Left (TypeError msg) -> do
      footnote msg
      assert $ L.all (`L.isInfixOf` msg) fragments

-- | Check that the constraint is satisfied.
noTypeError :: Dict c -> Property
noTypeError d = withTests 1 $ property do
  res <- evalIO $ try @TypeError $ deepForce 100 d
  case res of
    Right _ -> success
    Left (TypeError msg) -> do
      footnote msg
      failure

type RetInsert r r' = Dict (ReturningMatchesInsert (AnnSch "root") r r')
type RetUpsert r r' = Dict (ReturningMatchesUpsert (AnnSch "root") r r')
type RetUpdate r r' = Dict (ReturningMatchesUpdate (AnnSch "root") r r')

mustBeMaybe :: [String]
mustBeMaybe = ["Returning must use Maybe here"]

mustNotBeMaybe :: [String]
mustNotBeMaybe = ["Returning must not use Maybe here"]

-- Rows of the test schema --------------------------------------------------

type RootFull = "code" := Text :. "grp" := Int32 :. "name" := Text

-- | Child @mid1@ with all mandatory columns (@root_id@ comes from the parent).
type Mid1Full = "flag" := Bool :. "pos" := Int32 :. "sortKey" := Int32

-- | Child @mid1@ with primary key only (plus one plain column).
type Mid1Key = "id" := Int32 :. "flag" := Bool

type Mid1Out = "id" := Int32 :. "flag" := Bool

-- | Root key + key-only @mid1@ children.
type RootMid1Key = "id" := Int32 :. "mid1_root_fk" := [Mid1Key]

-- | Root key + @mid2@ children (key only) with @leaf@ grandchildren (key only).
type RootMid2LeafKey =
  "id" := Int32
    :. "mid2_root_fk" :=
      [ "seq" := Int32 :. "kind" := Text
        :. "leaf_mid2_fk" := ["leaf_no" := Int32 :. "category" := Maybe Text] ]

-- updateJSON ---------------------------------------------------------------

-- | A child list after a plain column (the reported case).
prop_update_child_bare :: Property
prop_update_child_bare = typeErrorWith
  (mustBeMaybe <> ["mid1_root_fk", "Got: (\"id\" := Int32) :. (\"flag\" := Bool)"])
  (Dict :: RetUpdate RootMid1Key
    (Maybe ("id" := Int32 :. "mid1_root_fk" := [Mid1Out])))

-- | A child list as the first field of the returning row.
prop_update_child_bare_first :: Property
prop_update_child_bare_first = typeErrorWith (mustBeMaybe <> ["mid1_root_fk"])
  (Dict :: RetUpdate RootMid1Key
    (Maybe ("mid1_root_fk" := [Mid1Out] :. "id" := Int32)))

-- | A bare child list after a correct (Maybe) one.
prop_update_second_child_bare :: Property
prop_update_second_child_bare = typeErrorWith (mustBeMaybe <> ["mid2_root_fk"])
  (Dict :: RetUpdate
    ( "id" := Int32 :. "mid1_root_fk" := [Mid1Key]
      :. "mid2_root_fk" := ["seq" := Int32 :. "kind" := Text] )
    (Maybe ("id" := Int32 :. "mid1_root_fk" := [Maybe Mid1Out]
      :. "mid2_root_fk" := ["seq" := Int32])))

-- | A bare grandchild list under a correct (Maybe) child list.
prop_update_grandchild_bare :: Property
prop_update_grandchild_bare = typeErrorWith
  (mustBeMaybe <> ["leaf_mid2_fk", "[\"mid2_root_fk\"]"])
  (Dict :: RetUpdate RootMid2LeafKey
    (Maybe ("id" := Int32 :. "mid2_root_fk" :=
      [Maybe ("seq" := Int32 :. "leaf_mid2_fk" := ["leaf_no" := Int32])])))

-- | Child list as the first field (the element was checked as @PgTag ann t@,
-- which is never @Maybe@).
prop_update_child_first_ok :: Property
prop_update_child_first_ok = noTypeError
  (Dict :: RetUpdate RootMid1Key
    (Maybe ("mid1_root_fk" := [Maybe Mid1Out] :. "id" := Int32)))

prop_update_ok :: Property
prop_update_ok = noTypeError
  (Dict :: RetUpdate RootMid2LeafKey
    (Maybe ("id" := Int32 :. "mid2_root_fk" :=
      [Maybe ("seq" := Int32 :. "leaf_mid2_fk" := [Maybe ("leaf_no" := Int32)])])))

-- insertJSON ---------------------------------------------------------------

prop_insert_child_maybe :: Property
prop_insert_child_maybe = typeErrorWith (mustNotBeMaybe <> ["mid1_root_fk"])
  (Dict :: RetInsert ("mid1_root_fk" := [Mid1Full] :. RootFull)
    ("id" := Int32 :. "mid1_root_fk" := [Maybe ("id" := Int32)]))

prop_insert_ok :: Property
prop_insert_ok = noTypeError
  (Dict :: RetInsert ("mid1_root_fk" := [Mid1Full] :. RootFull)
    ("id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Full]))

-- upsertJSON ---------------------------------------------------------------

-- | Key-only child in the input: a missing child is returned as @null@, even if
-- the returning row selects all mandatory columns.
prop_upsert_key_only_child_bare :: Property
prop_upsert_key_only_child_bare = typeErrorWith (mustBeMaybe <> ["mid1_root_fk"])
  (Dict :: RetUpsert RootMid1Key
    (Maybe ("id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Full])))

-- | Child with all mandatory columns in the input: it is always inserted or
-- updated, even if the returning row selects only the key.
prop_upsert_full_child_maybe :: Property
prop_upsert_full_child_maybe = typeErrorWith (mustNotBeMaybe <> ["mid1_root_fk"])
  (Dict :: RetUpsert ("id" := Int32 :. "mid1_root_fk" := [Mid1Full])
    (Maybe ("id" := Int32 :. "mid1_root_fk" := [Maybe ("id" := Int32)])))

-- | Key-only root in the input: a missing root is returned as @null@.
prop_upsert_key_only_root_bare :: Property
prop_upsert_key_only_root_bare = typeErrorWith
  ["Upsert returning row must be wrapped in Maybe", "\"code\""]
  (Dict :: RetUpsert ("id" := Int32 :. "name" := Text)
    ("id" := Int32 :. "name" := Text))

prop_upsert_ok :: Property
prop_upsert_ok = noTypeError
  (Dict :: RetUpsert RootMid1Key
    (Maybe ("id" := Int32 :. "mid1_root_fk" := [Maybe ("id" := Int32 :. Mid1Full)])))

prop_upsert_full_ok :: Property
prop_upsert_full_ok = noTypeError
  (Dict :: RetUpsert ("mid1_root_fk" := [Mid1Full] :. RootFull)
    ("id" := Int32 :. "mid1_root_fk" := ["id" := Int32]))

-- | Child input has only key columns and parent FK is outside the key:
-- @ON CONFLICT DO NOTHING@ ('NeedsFkKeyOnlyMaybe').
prop_upsert_do_nothing_child_bare :: Property
prop_upsert_do_nothing_child_bare = typeErrorWith (mustBeMaybe <> ["arrays_root_fk"])
  (Dict :: RetUpsert ("id" := Int32 :. "arrays_root_fk" := ["id" := Int32])
    (Maybe ("id" := Int32 :. "arrays_root_fk" := ["id" := Int32])))

-- | Child input has plain columns outside the key: @ON CONFLICT (key) DO UPDATE@
-- always returns the row.
prop_upsert_do_update_child_ok :: Property
prop_upsert_do_update_child_ok = noTypeError
  (Dict :: RetUpsert ("id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Full])
    (Maybe ("id" := Int32 :. "mid1_root_fk" := ["id" := Int32])))
