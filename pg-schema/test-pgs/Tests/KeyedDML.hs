{-# LANGUAGE BlockArguments #-}
-- |
-- Flat keyed DML: 'upsertByKey' needs mandatory fields and a full key
-- ('INSERT … ON CONFLICT'); key-only patches use 'updateByKey' only.
module Tests.KeyedDML where

import Control.Monad (void, when, unless)
import Data.Int (Int32)
import Data.List qualified as L
import Data.Maybe (Maybe(..))
import Data.Pool as Pool
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgSchema.DML
import Sch
import Utils

type RootRec =
  "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

type RootRetBare = "id" := Int32 :. RootRec

type RootKeyPatch =
  "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

type RootKeyRet = Maybe ("id" := Int32 :. "name" := Text)

type RootSel = "code" := Text :. "grp" := Int32 :. "name" := Text

type NullableRow =
  "code" := Text :. "suffix" := Maybe Text :. "name" := Text
    :. "note" := Maybe Text :. "someEmpty" := ()

type NullableKeyPatch =
  "code" := Text :. "suffix" := Maybe Text :. "note" := Maybe Text
    :. "someEmpty" := ()

type NullableKeyRet = Maybe ("id" := Int32 :. "note" := Maybe Text)

type RootPatchOnly =
  "id" := Int32 :. "code" := Text :. "grp" := Int32 :. "name" := Text
    :. "someEmpty" := ()

type RootKeyOnlyPatch = "id" := Int32 :. "name" := Text

type RootUpsOutMaybe = Maybe ("id" := Int32 :. "name" := Text)

type RootUpdOutMaybe = RootUpsOutMaybe

type RootUpsBare = "id" := Int32 :. RootRec

type Mid1Rec =
  "flag" := Bool :. "pos" := Int32 :. "sortKey" := Int32 :. "payload" := Maybe Text

type RootWithMid1 = "mid1_root_fk" := [Mid1Rec] :. RootRec

type RootInsWithMid =
  "id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Rec]

type Mid1KeyPatch = "id" := Int32 :. Mid1Rec

type Mid1KeyRet = Maybe ("id" := Int32 :. "flag" := Bool)

expectAbsent :: Show a => [Maybe a] -> IO ()
expectAbsent [Nothing] = pure ()
expectAbsent xs = fail $ "expected [Nothing], got " <> show xs

expectUpsertSql :: Text -> IO ()
expectUpsertSql sql = do
  let lower = T.toLower sql
  unless ("insert into" `T.isInfixOf` lower) $
    fail $ "expected INSERT upsert SQL, got: " <> T.unpack sql
  unless ("on conflict" `T.isInfixOf` lower) $
    fail $ "expected ON CONFLICT in upsert SQL, got: " <> T.unpack sql
  when (T.stripPrefix "update " (T.stripStart lower) == Just "") $
    fail $ "upsertByKey must not emit UPDATE-only SQL: " <> T.unpack sql

prop_upsert_by_key_insert_then_update :: Pool Connection -> Property
prop_upsert_by_key_insert_then_update pool = withTests 10 $ property do
  code <- forAll (Gen.text (Range.linear 3 20) Gen.alphaNum)
  grp <- forAll (Gen.int32 (Range.linear 1 10000))
  name1 <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
  name2 <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
  when (name1 == name2) discard
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn ("code" =? code)
    let
      full :: RootRec
      full = "code" =: code :. "grp" =: grp :. "name" =: name1 :. "someEmpty" =: ()
      row2 :: RootRec
      row2 = "code" =: code :. "grp" =: grp :. "name" =: name2 :. "someEmpty" =: ()
    ([_ :. _], sql1) <- upsertByKey (AnnSch "root") @RootRec @RootRetBare conn [full]
    expectUpsertSql sql1
    (ret, sql2) <- upsertByKey (AnnSch "root") @RootRec @RootKeyRet conn [row2]
    expectUpsertSql sql2
    case ret of
      [Just (_ :. n)] -> when (unPgTag n /= name2) $
        fail $ "expected name " <> show name2
      _ -> fail "expected [Just] from upsertByKey on conflict update"
    (rows :: [RootSel], _) <- selSch "root" conn $
      qRoot $ qWhere $ "code" =? code &&& "grp" =? grp
    case rows of
      [_ :. _ :. n] -> when (unPgTag n /= name2) $ fail "DB name not updated"
      _ -> fail "expected one row"

prop_upsert_by_key_composite_unique :: Pool Connection -> Property
prop_upsert_by_key_composite_unique pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 24) Gen.alphaNum)
  name1 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  name2 <- forAll (Gen.text (Range.linear 1 24) Gen.alphaNum)
  when (name1 == name2) discard
  let grp = 5151 :: Int32
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn ("code" =? code)
    let
      row1 :: RootRec
      row1 = "code" =: code :. "grp" =: grp :. "name" =: name1 :. "someEmpty" =: ()
      row2 :: RootRec
      row2 = "code" =: code :. "grp" =: grp :. "name" =: name2 :. "someEmpty" =: ()
    (_, sql1) <- upsByKey_ "root" conn [row1]
    expectUpsertSql sql1
    (_, sql2) <- upsByKey_ "root" conn [row2]
    expectUpsertSql sql2
    (rows :: [RootSel], _) <- selSch "root" conn $
      qRoot $ qWhere $ "code" =? code &&& "grp" =? grp
    case rows of
      [_ :. _ :. n] ->
        when (unPgTag n /= name2) $
          fail $ "expected updated name " <> show name2
      _ -> fail $ "expected 1 row, got " <> show (L.length rows)

prop_upsert_by_key_never_pure_update :: Pool Connection -> Property
prop_upsert_by_key_never_pure_update pool = withTests 3 $ property do
  code <- forAll (Gen.text (Range.linear 3 20) Gen.alphaNum)
  grp <- forAll (Gen.int32 (Range.linear 1 10000))
  name <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn ("code" =? code)
    let
      row :: RootRec
      row = "code" =: code :. "grp" =: grp :. "name" =: name :. "someEmpty" =: ()
    (_, sqlExec) <- upsByKey_ "root" conn [row]
    expectUpsertSql sqlExec
    let sqlText = upsertByKeyText_ (AnnSch "root") @RootRec
    expectUpsertSql sqlText

prop_update_by_key_found :: Pool Connection -> Property
prop_update_by_key_found pool = withTests 10 $ property do
  code <- forAll (Gen.text (Range.linear 3 20) Gen.alphaNum)
  note1 <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
  note2 <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
  when (note1 == note2) discard
  let sfx = Just ("sfx" :: Text)
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "nullable_uq_row" conn ("code" =? code)
    let
      seed :: NullableRow
      seed =
        "code" =: code :. "suffix" =: sfx :. "name" =: "n"
          :. "note" =: Just note1 :. "someEmpty" =: ()
      patch :: NullableKeyPatch
      patch =
        "code" =: code :. "suffix" =: sfx :. "note" =: Just note2
          :. "someEmpty" =: ()
    void $ insSch_ "nullable_uq_row" conn [seed]
    (ret, _) <- updateByKey (AnnSch "nullable_uq_row") @NullableKeyPatch
      @NullableKeyRet conn [patch]
    case ret of
      [Just (_ :. n)] -> when (unPgTag n /= Just note2) $ fail "note not updated"
      _ -> fail "expected [Just]"
    (rows :: ["note" := Maybe Text], _) <- selSch "nullable_uq_row" conn $
      qRoot $ qWhere $ "code" =? code &&& "suffix" =?? sfx
    case rows of
      [n] -> when (unPgTag n /= Just note2) $ fail "DB note mismatch"
      _ -> fail "expected one row"

prop_update_by_key_not_found :: Pool Connection -> Property
prop_update_by_key_not_found pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 20) Gen.alphaNum)
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "nullable_uq_row" conn ("code" =? code)
    let
      patch :: NullableKeyPatch
      patch =
        "code" =: code :. "suffix" =: Just "x" :. "note" =: Just "y"
          :. "someEmpty" =: ()
    (ret, _) <- updateByKey (AnnSch "nullable_uq_row") @NullableKeyPatch
      @NullableKeyRet conn [patch]
    expectAbsent ret
    (rows :: [NullableRow], _) <- selSch "nullable_uq_row" conn $
      qRoot $ qWhere $ "code" =? code
    unless (null rows) $ fail $ "expected 0 rows, got " <> show rows

prop_update_by_key_never_inserts :: Pool Connection -> Property
prop_update_by_key_never_inserts pool = withTests 3 $ property do
  code <- forAll (Gen.text (Range.linear 3 20) Gen.alphaNum)
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn mempty
    let
      patch :: RootKeyPatch
      patch =
        "code" =: code :. "grp" =: (1 :: Int32) :. "name" =: "x"
          :. "someEmpty" =: ()
    (ret, _) <- updateByKey (AnnSch "root") @RootKeyPatch @RootKeyRet conn [patch]
    expectAbsent ret
    (rows :: [RootSel], _) <- selSch "root" conn qpEmpty
    unless (null rows) $ fail $ "expected 0 rows, got " <> show rows

prop_update_json_not_found_returns_nothing :: Pool Connection -> Property
prop_update_json_not_found_returns_nothing pool = withTests 5 $ property do
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn mempty
    let
      bad :: RootPatchOnly
      bad =
        "id" =: (999999999 :: Int32) :. "code" =: "x" :. "grp" =: 1
          :. "name" =: "y" :. "someEmpty" =: ()
    (rets, _) <- updateJSON (AnnSch "root") @RootPatchOnly @RootUpdOutMaybe conn [bad]
    expectAbsent rets

prop_update_json_found :: Pool Connection -> Property
prop_update_json_found pool = withTests 10 $ property do
  code <- forAll (Gen.text (Range.linear 3 16) Gen.alphaNum)
  grp <- forAll (Gen.int32 (Range.linear 1 5000))
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn mempty
    let
      row :: RootRec
      row = "code" =: code :. "grp" =: grp :. "name" =: "seed" :. "someEmpty" =: ()
    (insOut, _) <- insertJSON (AnnSch "root") @RootRec @RootRetBare conn [row]
    insHead <- case insOut of
      (x : _) -> pure x
      [] -> fail "insertJSON returned no rows"
    let
      (PgTag rid :. _) = insHead
      patch :: RootPatchOnly
      patch =
        "id" =: rid :. "code" =: code :. "grp" =: grp :. "name" =: "patched"
          :. "someEmpty" =: ()
    (ret, _) <- updateJSON (AnnSch "root") @RootPatchOnly @RootUpdOutMaybe conn [patch]
    case ret of
      [Just (_ :. PgTag n)] -> when (n /= "patched") $ fail "name not updated"
      _ -> fail $ "unexpected updateJSON result: " <> show ret

prop_upsert_json_returning_positions :: Pool Connection -> Property
prop_upsert_json_returning_positions pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 16) Gen.alphaNum)
  grp <- forAll (Gen.int32 (Range.linear 1 5000))
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn mempty
    let
      row :: RootRec
      row = "code" =: code :. "grp" =: grp :. "name" =: "n" :. "someEmpty" =: ()
    (insOut, _) <- insertJSON (AnnSch "root") @RootRec @RootRetBare conn [row]
    insHead <- case insOut of
      (x : _) -> pure x
      [] -> fail "insertJSON returned no rows"
    let
      (PgTag rid :. _) = insHead
      good :: RootKeyOnlyPatch
      good = "id" =: rid :. "name" =: "ok"
      bad :: RootKeyOnlyPatch
      bad = "id" =: (888888888 :: Int32) :. "name" =: "z"
    (rets, _) <- upsertJSON (AnnSch "root") @RootKeyOnlyPatch @RootUpsOutMaybe conn [good, bad]
    when (length rets /= 2) $ fail $ "expected 2 results, got " <> show rets
    case rets of
      [Just _, Nothing] -> pure ()
      _ -> fail $ "expected [Just, Nothing], got " <> show rets

prop_upsert_json_full_row_bare_returning :: Pool Connection -> Property
prop_upsert_json_full_row_bare_returning pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 16) Gen.alphaNum)
  grp <- forAll (Gen.int32 (Range.linear 1 5000))
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "root" conn mempty
    let
      row :: RootRec
      row = "code" =: code :. "grp" =: grp :. "name" =: "n" :. "someEmpty" =: ()
    (rets, _) <- upsertJSON (AnnSch "root") @RootRec @RootUpsBare conn [row]
    case rets of
      [PgTag _ :. _] -> pure ()
      _ -> fail "expected bare upsert returning row"

-- | Root @Just@, key-only upsert patch on child table via flat @updateByKey@.
prop_update_json_child_maybe :: Pool Connection -> Property
prop_update_json_child_maybe pool = withTests 5 $ property do
  code <- forAll (Gen.text (Range.linear 3 16) Gen.alphaNum)
  grp <- forAll (Gen.int32 (Range.linear 1 5000))
  evalIO $ Pool.withResource pool \conn -> do
    void $ delByCond "mid1" conn mempty
    void $ delByCond "root" conn mempty
    let
      row :: RootRec
      row = "code" =: code :. "grp" =: grp :. "name" =: "n" :. "someEmpty" =: ()
    ([_ :. PgTag mids], _) <- insertJSON (AnnSch "root") @RootWithMid1 @RootInsWithMid conn
      [ "mid1_root_fk" =: [ "flag" =: True :. "pos" =: 1 :. "sortKey" =: 1
          :. "payload" =: Nothing ] :. row ]
    let
      midId = case mids of [PgTag i :. _] -> i; _ -> error "one mid1"
      patch :: Mid1KeyPatch
      patch = "id" =: (midId + 99999) :. "flag" =: False :. "pos" =: 1
        :. "sortKey" =: 1 :. "payload" =: Nothing
    (ret, _) <- updateByKey (AnnSch "mid1") @Mid1KeyPatch @Mid1KeyRet conn [patch]
    expectAbsent ret
