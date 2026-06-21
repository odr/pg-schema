{-# LANGUAGE BlockArguments #-}
module Tests.UpsertUniqueKey where

import Control.Monad (void, when)
import Data.Int (Int32)
import Data.List qualified as L
import Data.Pool as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Hedgehog (Property, discard, evalIO, forAll, property, withTests)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgSchema.DML
import Utils

type RootRec =
  "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

type RootSel = "code" := Text :. "grp" := Int32 :. "name" := Text

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
    void $ delByCond "root" conn ("code" =? code)
    let
      row1 :: RootRec
      row1 = "code" =: code :. "grp" =: grp :. "name" =: name1 :. "someEmpty" =: ()
      row2 :: RootRec
      row2 = "code" =: code :. "grp" =: grp :. "name" =: name2 :. "someEmpty" =: ()
    void $ upsJSON_ "root" conn [row1]
    void $ upsJSON_ "root" conn [row2]
    (rows :: [RootSel], _) <- selSch "root" conn $
      qRoot $ qWhere $ "code" =? code &&& "grp" =? grp
    case rows of
      [_ :. _ :. n] ->
        when (unPgTag n /= name2) $
          fail $ "expected updated name " <> show name2 <> ", got " <> show (unPgTag n)
      _ -> fail $ "expected 1 row, got " <> show (L.length rows)

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
    void $ delByCond "nullable_uq_row" conn ("code" =? code)
    let
      row1 = "code" =: code :. "suffix" =: sfx :. "name" =: name1
        :. "note" =: (Nothing :: Maybe Text) :. "someEmpty" =: ()
      row2 = "code" =: code :. "suffix" =: sfx :. "name" =: name2
        :. "note" =: (Nothing :: Maybe Text) :. "someEmpty" =: ()
    void $ upsJSON_ "nullable_uq_row" conn [row1]
    void $ upsJSON_ "nullable_uq_row" conn [row2]
    (rows :: ["name" := Text], _) <- selSch "nullable_uq_row" conn $
      qRoot $ qWhere $ "code" =? code &&& "suffix" =?? sfx
    case rows of
      [n] ->  when (unPgTag n /= name2) $ fail
        $ "expected updated name " <> show name2 <> ", got " <> show (unPgTag n)
      _ -> fail $ "expected 1 row, got " <> show (L.length rows)

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
    void $ delByCond "nullable_uq_row" conn ("code" =? code)
    let
      seed =
        "code" =: code :. "suffix" =: sfx :. "name" =: fullName
          :. "note" =: Just note1 :. "someEmpty" =: ()
      patch =
        "code" =: code :. "suffix" =: sfx :. "note" =: Just note2
          :. "someEmpty" =: ()
    void $ upsJSON_ "nullable_uq_row" conn [seed]
    void $ upsJSON_ "nullable_uq_row" conn [patch]
    (rows :: ["name" := Text :. "note" := Maybe Text], _) <-
      selSch "nullable_uq_row" conn
        $ qRoot $ qWhere $ "code" =? code &&& "suffix" =?? sfx
    case rows of
      [nm :. note] -> do
        when (unPgTag note /= Just note2) $
          fail $ "expected note " <> show note2 <> ", got " <> show (unPgTag note)
        when (unPgTag nm /= fullName) $
          fail $ "name should be unchanged, got " <> show (unPgTag nm)
      _ -> fail $ "expected 1 row, got " <> show (L.length rows)

-- | NULL in nullable key column: classic UNIQUE allows duplicate keys.
prop_upsert_json_nullable_key_null_inserts_twice :: Pool Connection -> Property
prop_upsert_json_nullable_key_null_inserts_twice pool = withTests 3 $ property do
  code <- forAll (Gen.text (Range.linear 3 24) Gen.alphaNum)
  name1 <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
  name2 <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
  when (name1 == name2) discard
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "nullable_uq_row" conn ("code" =? code)
    let
      row nm =
        "code" =: code :. "suffix" =: (Nothing :: Maybe Text) :. "name" =: nm
          :. "note" =: (Nothing :: Maybe Text) :. "someEmpty" =: ()
    void $ upsJSON_ "nullable_uq_row" conn [row name1]
    void $ upsJSON_ "nullable_uq_row" conn [row name2]
    (rows :: ["code" := Text], _) <- selSch "nullable_uq_row" conn $
      qRoot $ qWhere $ "code" =? code &&& pnull "suffix"
    when (L.length rows /= 2) $
      fail $ "expected 2 rows with null suffix, got " <> show (L.length rows)
