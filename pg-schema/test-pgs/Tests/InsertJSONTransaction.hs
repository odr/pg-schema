{-# LANGUAGE BlockArguments #-}
module Tests.InsertJSONTransaction where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.Functor
import Data.Int (Int32, Int64)
import Data.List qualified as L
import Data.Pool as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Hedgehog
import Utils
import PgSchema.DML

type RootRec = "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

type RootSel = "code" := Text :. "grp" := Int32 :. "name" := Text

eqRoot :: RootRec -> RootRec -> Bool
eqRoot (a1 :. b1 :. c1) (a2 :. b2 :. c2) = (a1, b1) == (a2, b2)

countRoots :: Connection -> IO Int64
countRoots conn = do
  [cnt] <- fst <$> selSch "root" conn qpEmpty
  pure $ unAggr @ACount $ unPgTag @"cnt" cnt

countRootsCode :: Connection -> Text -> IO Int64
countRootsCode conn code = do
  [cnt] <- fst <$> selSch "root" conn (qRoot $ qWhere $ "code" =? code)
  pure $ unAggr @ACount $ unPgTag @"cnt" cnt

clearRoots :: Connection -> IO ()
clearRoots conn = void $ delByCond "root" conn mempty

prop_insertJSON_standalone_commits :: Pool Connection -> Property
prop_insertJSON_standalone_commits pool = property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 5)
  evalIO do
    Pool.withResource pool clearRoots
    Pool.withResource pool \conn -> void $ insJSON_ "root" conn rootsIn
    Pool.withResource pool \conn -> do
      n <- countRoots conn
      when (n /= fromIntegral (length rootsIn)) $
        fail $ "expected " <> show (length rootsIn) <> " rows, got " <> show n

prop_insertJSON_respects_outer_transaction :: Pool Connection -> Property
prop_insertJSON_respects_outer_transaction pool = property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 5)
  evalIO do
    Pool.withResource pool clearRoots
    Pool.withResource pool \conn -> do
      execute_ conn "begin"
      void $ insJSON_ "root" conn rootsIn
      n <- countRoots conn
      when (n /= fromIntegral (length rootsIn)) $
        fail $ "expected " <> show (length rootsIn) <> " rows in session, got "
          <> show n
      Pool.withResource pool \conn2 -> do
        n2 <- countRoots conn2
        when (n2 /= 0) $
          fail $ "uncommitted rows visible in other session: " <> show n2
      rollback conn
    Pool.withResource pool \conn -> do
      n <- countRoots conn
      when (n /= 0) $
        fail $ "rows remained after rollback: " <> show n

prop_insertJSON_begin_without_prior_writes :: Pool Connection -> Property
prop_insertJSON_begin_without_prior_writes pool = property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 5)
  evalIO do
    Pool.withResource pool clearRoots
    Pool.withResource pool \conn -> do
      execute_ conn "begin"
      void $ insJSON_ "root" conn rootsIn
      rollback conn
    Pool.withResource pool \conn -> do
      n <- countRoots conn
      when (n /= 0) $
        fail $ "rows remained after rollback from empty begin: " <> show n

prop_insertJSON_rolls_back_on_failure :: Pool Connection -> Property
prop_insertJSON_rolls_back_on_failure pool = property do
  evalIO do
    Pool.withResource pool clearRoots
    let
      row :: RootRec
      row = "code" =: "dup_tx" :. "grp" =: (1 :: Int32)
        :. "name" =: "x" :. "someEmpty" =: ()
    Pool.withResource pool \conn -> do
      res <- try @SomeException $ insJSON_ "root" conn [row, row]
      case res of
        Left _ -> pure ()
        Right _ -> fail "expected unique violation"
      n <- countRootsCode conn "dup_tx"
      when (n /= 0) $
        fail $ "partial insert remained after failure: " <> show n
    Pool.withResource pool \conn -> do
      n <- countRootsCode conn "dup_tx"
      when (n /= 0) $
        fail $ "failed insert visible in other session: " <> show n
