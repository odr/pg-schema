{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.UpsertUniqueKey where

import Control.Monad (void, when)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Pool as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Hedgehog (Property, discard, evalIO, forAll, property, withTests)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgSchema.DML
import Utils

type RootRec =
  "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

-- | Upsert by composite unique @(code, grp)@ when PK @id@ is omitted; second call
-- updates the same row.
prop_upsert_json_by_composite_unique :: Pool Connection -> Property
prop_upsert_json_by_composite_unique pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 24) Gen.alphaNum)
  name1 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  name2 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  when (name1 == name2) discard
  let grp = 4242 :: Int32
  evalIO $ Pool.withResource pool \conn -> do
    execute conn "delete from test_pgs.root where code = ?" (Only code)
    let
      row1 :: RootRec
      row1 = "code" =: code :. "grp" =: grp :. "name" =: name1 :. "someEmpty" =: ()
      row2 :: RootRec
      row2 = "code" =: code :. "grp" =: grp :. "name" =: name2 :. "someEmpty" =: ()
    void $ upsJSON_ "root" conn [row1]
    void $ upsJSON_ "root" conn [row2]
    [Only n] <- query conn
      "select name from test_pgs.root where code = ? and grp = ?"
      (code, grp)
    when (n /= name2) $
      fail $ "expected updated name " <> show name2 <> ", got " <> show n
    [Only cnt] <- query conn
      "select count(*)::int8 from test_pgs.root where code = ? and grp = ?"
      (code, grp)
    when (cnt /= 1) $
      fail $ "expected 1 row, got " <> show (cnt :: Int64)

type NullableUqRec =
  "code" := Text
    :. "suffix" := Maybe Text
    :. "name" := Text
    :. "note" := Maybe Text
    :. "someEmpty" := ()

type NullableUqPatch =
  "code" := Text
    :. "suffix" := Maybe Text
    :. "note" := Maybe Text
    :. "someEmpty" := ()

-- | Upsert by nullable unique @(code, suffix)@ when table has no NOT NULL UK.
prop_upsert_json_by_nullable_unique :: Pool Connection -> Property
prop_upsert_json_by_nullable_unique pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 24) Gen.alphaNum)
  suffix <- forAll (Gen.text (Range.linear 1 12) Gen.alphaNum)
  name1 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  name2 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  when (name1 == name2) discard
  let sfx = Just suffix
  evalIO $ Pool.withResource pool \conn -> do
    execute conn "delete from test_pgs.nullable_uq_row where code = ?" (Only code)
    let
      row1 :: NullableUqRec
      row1 =
        "code" =: code :. "suffix" =: sfx :. "name" =: name1
          :. "note" =: Nothing :. "someEmpty" =: ()
      row2 :: NullableUqRec
      row2 =
        "code" =: code :. "suffix" =: sfx :. "name" =: name2
          :. "note" =: Nothing :. "someEmpty" =: ()
    void $ upsJSON_ "nullable_uq_row" conn [row1]
    void $ upsJSON_ "nullable_uq_row" conn [row2]
    [Only n] <- query conn
      "select name from test_pgs.nullable_uq_row \
      \where code = ? and suffix is not distinct from ?"
      (code, suffix)
    when (n /= name2) $
      fail $ "expected updated name " <> show name2 <> ", got " <> show n
    [Only cnt] <- query conn
      "select count(*)::int8 from test_pgs.nullable_uq_row where code = ?"
      (Only code)
    when (cnt /= 1) $
      fail $ "expected 1 row, got " <> show (cnt :: Int64)

-- | UPDATE branch: key only + optional field, mandatory @name@ omitted.
prop_upsert_json_update_by_nullable_unique :: Pool Connection -> Property
prop_upsert_json_update_by_nullable_unique pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 24) Gen.alphaNum)
  suffix <- forAll (Gen.text (Range.linear 1 12) Gen.alphaNum)
  note1 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  note2 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  when (note1 == note2) discard
  let
    sfx = Just suffix
    fullName = "seed-name"
  evalIO $ Pool.withResource pool \conn -> do
    execute conn "delete from test_pgs.nullable_uq_row where code = ?" (Only code)
    let
      seed :: NullableUqRec
      seed =
        "code" =: code :. "suffix" =: sfx :. "name" =: fullName
          :. "note" =: Just note1 :. "someEmpty" =: ()
      patch :: NullableUqPatch
      patch =
        "code" =: code :. "suffix" =: sfx :. "note" =: Just note2
          :. "someEmpty" =: ()
    void $ upsJSON_ "nullable_uq_row" conn [seed]
    void $ upsJSON_ "nullable_uq_row" conn [patch]
    [Only n] <- query conn
      "select note from test_pgs.nullable_uq_row \
      \where code = ? and suffix is not distinct from ?"
      (code, suffix)
    when (fromMaybe "" n /= note2) $
      fail $ "expected note " <> show note2 <> ", got " <> show n
    [Only nm] <- query conn
      "select name from test_pgs.nullable_uq_row \
      \where code = ? and suffix is not distinct from ?"
      (code, suffix)
    when (nm /= fullName) $
      fail $ "name should be unchanged, got " <> show nm

-- | NULL in nullable key column: classic UNIQUE allows duplicate keys.
prop_upsert_json_nullable_key_null_inserts_twice :: Pool Connection -> Property
prop_upsert_json_nullable_key_null_inserts_twice pool = withTests 3 $ property do
  code <- forAll (Gen.text (Range.linear 3 24) Gen.alphaNum)
  name1 <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
  name2 <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
  when (name1 == name2) discard
  evalIO $ Pool.withResource pool \conn -> do
    execute conn "delete from test_pgs.nullable_uq_row where code = ?" (Only code)
    let
      row :: Text -> NullableUqRec
      row nm =
        "code" =: code :. "suffix" =: Nothing :. "name" =: nm
          :. "note" =: Nothing :. "someEmpty" =: ()
    void $ upsJSON_ "nullable_uq_row" conn [row name1]
    void $ upsJSON_ "nullable_uq_row" conn [row name2]
    [Only cnt] <- query conn
      "select count(*)::int8 from test_pgs.nullable_uq_row \
      \where code = ? and suffix is null"
      (Only code)
    when (cnt /= 2) $
      fail $ "expected 2 rows with null suffix, got " <> show (cnt :: Int64)
