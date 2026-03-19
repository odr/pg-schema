{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
module Tests.Hierarchy where

import Control.Monad (void)
import Data.Function (on)
import Data.Functor
import Data.Int (Int32, Int64)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Pool as Pool
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import GHC.Generics
import Hedgehog
import PgSchema.DML
import Sch
import Utils


type RootRec = "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

type Mid1Rec =
  "flag" := Bool :. "pos" := Int32 :. "sortKey" := Int32 :. "payload" := Maybe Text

data Mid2Rec = MkMid2Rec
  { seq :: Int32
  , kind :: Text
  , flag :: Bool
  , priority :: Int32 }
  deriving (Generic, Eq, Ord, Show)
  deriving anyclass GenDefault

data Mid2RecRev = MkMid2RecRev
  { seq :: Int32
  , kind :: Text
  , mid2RootFk :: RootRec
    :. "mid1_root_fk" := ["flag" := Bool]
    :. "mid1_root_fk2" := ["pos" := Int32]}
  deriving (Generic, Eq, Ord, Show)
  deriving anyclass GenDefault

data LeafI = MkLeafI
  { leafNo :: Int32
  , someEmpty :: ()
  , value :: Double }
  deriving (Generic, Eq, Ord, Show)
  deriving anyclass GenDefault

data Leaf = MkLeaf
  { leafNo :: Int32
  , value :: Double
  , someEmpty :: ()
  , category :: Maybe Text
  , leafMid2Fk :: Mid2Rec
  , leaf_mid2_rev_fk :: Mid2RecRev }
  deriving (Generic, Eq, Ord, Show)
  deriving anyclass GenDefault
-- >>> getRecordInfo @('Ann RenamerSch Sch (TS "leaf")) @LeafI
-- RecordInfo {tabName = NameNS {nnsNamespace = "test_pgs", nnsName = "leaf"}, fields = [FieldInfo {fieldName = "leaf_no", fieldDbName = "leaf_no", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "value", fieldDbName = "value", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "float4"}, fdNullable = False, fdHasDefault = False})}]}

eqRoot :: RootRec -> RootRec -> Bool
eqRoot (a1 :. b1 :. c1) (a2 :. b2 :. c2) = (a1, b1) == (a2, b2)

prop_hier_insert_simple_fk :: Pool Connection -> Property
prop_hier_insert_simple_fk pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid1In <- forAll (genData' Mid1Rec 0 20)
  let inIns = rootsIn <&> ("mid1RootFk" =: mid1In :.)
  (outIns, outSel1, outUps, outSel2) <- evalIO $ withPool pool \conn -> do
    delByCond "mid1" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["id" := Int32 :. "mid1RootFk" := ["id" := Int32 :. Mid1Rec]])) <-
      insJSON "root" conn inIns
    (fst -> (outSel1' :: ["id" := Int32 :. "mid1RootFk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    (outUps' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec], _txt) <-
      upsJSON "root" conn $ outIns' <&> \(ir :. PgTag ms) -> "mid1_root_fk" =:
        (ms <&> \(i :. flag :. x) -> i :. (not <$> flag) :. x) :. ir
    -- T.putStrLn $ "\n\n" <> txt <> "\n\n"
    (fst -> (outSel2' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    pure (outIns', outSel1', outUps', outSel2')
  L.sort (inIns <&> \(ms :. _) -> L.length ms) ===
    L.sort (outIns <&> \(_ :. ms) -> L.length ms)
  L.length outIns === L.length outSel1
  L.length outIns === L.length outSel2
  L.length outIns === L.length outUps
  (L.sort outIns <&> \(ir :. ms) -> ir :. length ms)
    === (L.sort outSel2 <&> \(ir :. ms :. _) -> ir :. length ms)
  (L.sort outIns <&> \(ir :. ms) -> ir :. length ms)
    === (L.sort outUps <&> \(ir :. ms :. _) -> ir :. length ms)
  (L.sort outIns <&> \(ir :. PgTag ms) -> ir :. length (filter (\(_ :. PgTag b :. _) -> b) ms))
    === (L.sort outSel2 <&> \(ir :. PgTag ms :. _) -> ir :. length (filter (\(PgTag b :. _) -> not b) ms))
  (L.sort outUps <&> \(ir :. PgTag ms :. _) -> ir :. length (filter (\(PgTag b :. _) -> b) ms))
    === (L.sort outSel2 <&> \(ir :. PgTag ms :. _) -> ir :. length (filter (\(PgTag b :. _) -> b) ms))

prop_hier_insert_composite_fk :: Pool Connection -> Property
prop_hier_insert_composite_fk pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genData' Mid2Rec 0 20)
  let inIns = rootsIn <&> (("mid2_root_fk" =: mid2In) :.)
  (outIns, outSel1, outUps, outSel2) <- evalIO $ withPool pool \conn -> do
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec]])) <-
      insJSON "root" conn inIns
    (fst -> (outSel1' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    (outUps' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec] :. RootRec], _txt) <-
      upsJSON "root" conn $ outIns' <&> \(ir :. PgTag ms) -> "mid2_root_fk" =:
        (ms <&> \m -> m { flag = not m.flag }) :. ir
    -- T.putStrLn $ "\n\n" <> txt <> "\n\n"
    (fst -> (outSel2' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    pure (outIns', outSel1', outUps', outSel2')
  L.sort (inIns <&> \(ms :. _) -> L.length ms) ===
    L.sort (outIns <&> \(_ :. ms) -> L.length ms)
  L.length outIns === L.length outSel1
  L.length outIns === L.length outSel2
  L.length outIns === L.length outUps
  (L.sort outIns <&> \(ir :. ms) -> ir :. length ms)
    === (L.sort outSel2 <&> \(ir :. ms :. _) -> ir :. length ms)
  (L.sort outIns <&> \(ir :. ms) -> ir :. length ms)
    === (L.sort outUps <&> \(ir :. ms :. _) -> ir :. length ms)
  (L.sort outIns <&> \(ir :. PgTag ms) -> ir :. length (filter (.flag) ms))
    === (L.sort outSel2 <&> \(ir :. PgTag ms :. _) -> ir :. length (filter (not . (.flag)) ms))
  (L.sort outUps <&> \(ir :. PgTag ms :. _) -> ir :. length (filter (.flag) ms))
    === (L.sort outSel2 <&> \(ir :. PgTag ms :. _) -> ir :. length (filter (.flag) ms))

prop_hier_select_child_with_parent :: Pool Connection -> Property
prop_hier_select_child_with_parent pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genData' Mid2Rec 0 20)
  (outIns, outSel) <- evalIO $ withPool pool \conn -> do
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["mid2_root_fk" := [Mid2Rec] :. RootRec])) <-
      insJSON "root" conn $ rootsIn <&> (("mid2_root_fk" =: mid2In) :.)
    (fst -> (outSel' :: [Mid2Rec :. "mid2_root_fk" := RootRec])) <-
      selSch "mid2" conn qpEmpty
    pure (outIns', outSel')
  L.sort (outSel <&> \(m2r :. PgTag rr) -> m2r :. rr) ===
    L.sort (outIns >>= \(PgTag m2rs :. rr) -> m2rs <&> (:. rr))

prop_hier_duplicate_names_root_nested :: Pool Connection -> Property
prop_hier_duplicate_names_root_nested pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 100)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genData' Mid2Rec 0 10)
  leafIn <- forAll (L.nubBy ((==) `on` (.leafNo)) <$> genData' LeafI 0 10)
  let inIns = rootsIn <&> (("mid2_root_fk" =: (mid2In <&> (("leaf_mid2_fk" =: leafIn) :.))) :.)
  outSel <- evalIO $ withPool pool \conn -> do
    delByCond "leaf" conn mempty
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    void $ insJSON_ "root" conn inIns
    (fst -> (outSel' :: [Leaf])) <- selSch "leaf" conn qpEmpty
    pure outSel'
  L.length outSel === L.length (inIns >>= \(PgTag xs :. _) -> xs >>= \(PgTag ys :. _) -> ys )
