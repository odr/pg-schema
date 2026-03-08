{-# LANGUAGE BlockArguments #-}
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
import Data.Tagged (Tagged(..), unTagged)
import Data.Text (Text)
import Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import GHC.Generics
import Hedgehog
import PgSchema
import PgSchema.Utils
import Sch
import Utils


type RootRec = "code" := Text :. "grp" := Int32 :. "name" := Text

type Mid1Rec =
  "flag" := Bool :. "pos" := Int32 :. "sort_key" := Int32 :. "payload" := Maybe Text

data Mid2Rec = MkMid2Rec
  { seq :: Int32
  , kind :: Text
  , flag :: Bool
  , priority :: Int32 }
  deriving (Generic, Eq, Ord, Show)

data Mid2RecRev = MkMid2RecRev
  { seq :: Int32
  , kind :: Text
  , mid2_root_fk :: RootRec
    :. "mid1_root_fk" := ["flag" := Bool]
    :. "mid1_root_fk2" := ["pos" := Int32]}
  deriving (Generic, Eq, Ord, Show)

data LeafI = MkLeafI
  { leaf_no :: Int32
  , value :: Double }
  deriving (Generic, Eq, Ord, Show)

data Leaf = MkLeaf
  { leaf_no :: Int32
  , value :: Double
  , category :: Maybe Text
  , leaf_mid2_fk :: Mid2Rec
  , leaf_mid2_rev_fk :: Mid2RecRev }
  deriving (Generic, Eq, Ord, Show)
-- >>> getRecordInfo @Sch @(TS "leaf") @(HSch "leaf" Leaf)
-- RecordInfo {tabName = NameNS {nnsNamespace = "test_pgs", nnsName = "leaf"}, fields = [FieldInfo {fieldName = "leaf_no", fieldDbName = "leaf_no", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "value", fieldDbName = "value", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "numeric"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "category", fieldDbName = "category", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = True, fdHasDefault = False})},FieldInfo {fieldName = "leaf_mid2_fk", fieldDbName = "leaf_mid2_fk", fieldKind = RFFromHere (RecordInfo {tabName = NameNS {nnsNamespace = "test_pgs", nnsName = "mid2"}, fields = [FieldInfo {fieldName = "seq", fieldDbName = "seq", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "kind", fieldDbName = "kind", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "flag", fieldDbName = "flag", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "bool"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "priority", fieldDbName = "priority", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})}]}) [Ref {fromName = "root_id", fromDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}, toName = "root_id", toDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}},Ref {fromName = "seq", fromDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}, toName = "seq", toDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}}]},FieldInfo {fieldName = "leaf_mid2_fk___1", fieldDbName = "leaf_mid2_fk", fieldKind = RFFromHere (RecordInfo {tabName = NameNS {nnsNamespace = "test_pgs", nnsName = "mid2"}, fields = [FieldInfo {fieldName = "seq", fieldDbName = "seq", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "kind", fieldDbName = "kind", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "mid2_root_fk", fieldDbName = "mid2_root_fk", fieldKind = RFFromHere (RecordInfo {tabName = NameNS {nnsNamespace = "test_pgs", nnsName = "root"}, fields = [FieldInfo {fieldName = "code", fieldDbName = "code", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "grp", fieldDbName = "grp", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})},FieldInfo {fieldName = "name", fieldDbName = "name", fieldKind = RFPlain (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = False, fdHasDefault = False})}]}) [Ref {fromName = "root_id", fromDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}, toName = "id", toDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = True}}]}]}) [Ref {fromName = "root_id", fromDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}, toName = "root_id", toDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}},Ref {fromName = "seq", fromDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}, toName = "seq", toDef = FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False}}]}]}

eqRoot :: RootRec -> RootRec -> Bool
eqRoot (a1 :. b1 :. c1) (a2 :. b2 :. c2) = (a1, b1) == (a2, b2)

prop_hier_insert_simple_fk :: Pool Connection -> Property
prop_hier_insert_simple_fk pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid1In <- forAll (genData' Mid1Rec 0 20)
  let inIns = rootsIn <&> ("mid1_root_fk" =: mid1In :.)
  (outIns, outSel1, outUps, outSel2) <- evalIO $ withPool pool \conn -> do
    delByCond "mid1" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Rec]])) <-
      insJSON "root" conn inIns
    (fst -> (outSel1' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    (outUps' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec], _txt) <-
      upsJSON "root" conn $ outIns' <&> \(ir :. Tagged ms) -> "mid1_root_fk" =:
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
  (L.sort outIns <&> \(ir :. Tagged ms) -> ir :. length (filter (\(_ :. Tagged b :. _) -> b) ms))
    === (L.sort outSel2 <&> \(ir :. Tagged ms :. _) -> ir :. length (filter (\(Tagged b :. _) -> not b) ms))
  (L.sort outUps <&> \(ir :. Tagged ms :. _) -> ir :. length (filter (\(Tagged b :. _) -> b) ms))
    === (L.sort outSel2 <&> \(ir :. Tagged ms :. _) -> ir :. length (filter (\(Tagged b :. _) -> b) ms))

prop_hier_insert_composite_fk :: Pool Connection -> Property
prop_hier_insert_composite_fk pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genDataH "mid2" Mid2Rec 0 20)
  let inIns = rootsIn <&> (("mid2_root_fk" =: mid2In) :.)
  (outIns, outSel1, outUps, outSel2) <- evalIO $ withPool pool \conn -> do
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec]])) <-
      insJSON "root" conn inIns
    (fst -> (outSel1' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    (outUps' :: ["id" := Int32 :. "mid2_root_fk" := [Mid2Rec] :. RootRec], _txt) <-
      upsJSON "root" conn $ outIns' <&> \(ir :. Tagged ms) -> "mid2_root_fk" =:
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
  (L.sort outIns <&> \(ir :. Tagged ms) -> ir :. length (filter (.flag) ms))
    === (L.sort outSel2 <&> \(ir :. Tagged ms :. _) -> ir :. length (filter (not . (.flag)) ms))
  (L.sort outUps <&> \(ir :. Tagged ms :. _) -> ir :. length (filter (.flag) ms))
    === (L.sort outSel2 <&> \(ir :. Tagged ms :. _) -> ir :. length (filter (.flag) ms))

prop_hier_select_child_with_parent :: Pool Connection -> Property
prop_hier_select_child_with_parent pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genDataH "mid2" Mid2Rec 0 20)
  (outIns, outSel) <- evalIO $ withPool pool \conn -> do
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["mid2_root_fk" := [Mid2Rec] :. RootRec])) <-
      insJSON "root" conn $ rootsIn <&> (("mid2_root_fk" =: mid2In) :.)
    (fst -> (outSel' :: [Mid2Rec :. "mid2_root_fk" := RootRec])) <-
      selSch "mid2" conn qpEmpty
    pure (outIns', outSel')
  L.sort (outSel <&> \(m2r :. Tagged rr) -> m2r :. rr) ===
    L.sort (outIns >>= \(Tagged m2rs :. rr) -> m2rs <&> (:. rr))

prop_hier_duplicate_names_root_nested :: Pool Connection -> Property
prop_hier_duplicate_names_root_nested pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 100)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genDataH "mid2" Mid2Rec 0 10)
  leafIn <- forAll (L.nubBy ((==) `on` (.leaf_no)) <$> genDataH "leaf" LeafI 0 10)
  let inIns = rootsIn <&> (("mid2_root_fk" =: (mid2In <&> (("leaf_mid2_fk" =: leafIn) :.))) :.)
  outSel <- evalIO $ withPool pool \conn -> do
    delByCond "leaf" conn mempty
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    void $ insJSON_ "root" conn inIns
    (fst -> (outSel' :: [Leaf])) <- selSch "leaf" conn qpEmpty
    pure outSel'
  L.length outSel === L.length (inIns >>= \(Tagged xs :. _) -> xs >>= \(Tagged ys :. _) -> ys )
