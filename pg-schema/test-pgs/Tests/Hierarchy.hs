{-# LANGUAGE BlockArguments #-}
module Tests.Hierarchy where

import Control.Monad (void)
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
import Hedgehog
import PgSchema
import PgSchema.Util
import Sch
import Utils


type RootRec = "code" := Text :. "grp" := Int32 :. "name" := Text

type Mid1Rec =
  "flag" := Bool :. "pos" := Int32 :. "sort_key" := Int32 :. "payload" := Maybe Text

eqRoot :: RootRec -> RootRec -> Bool
eqRoot (a1 :. b1 :. c1) (a2 :. b2 :. c2) = (a1, b1) == (a2, b2)

prop_hier_insert_simple_fk :: Pool Connection -> Property
prop_hier_insert_simple_fk pool = withTests 30 $ property do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootRec 1 200)
  mid1In <- forAll (genData' Mid1Rec 0 20)
  (outIns, outSel1, outUps, outSel2) <- evalIO $ withPool pool \conn -> do
    delByCond "mid1" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Rec]])) <-
      insJSON "root" conn $ rootsIn <&> (("mid1_root_fk" =: mid1In) :.)
    (fst -> (outSel1' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    (outUps' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec], _txt) <-
      upsJSON "root" conn $ outIns' <&> \(ir :. Tagged ms) -> "mid1_root_fk" =:
        (ms <&> \(i :. flag :. x) -> i :. (not <$> flag) :. x) :. ir
    -- T.putStrLn $ "\n\n" <> txt <> "\n\n"
    (fst -> (outSel2' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    pure (outIns', outSel1', outUps', outSel2')
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
  mid1In <- forAll (genData' Mid1Rec 0 20)
  (outIns, outSel1, outUps, outSel2) <- evalIO $ withPool pool \conn -> do
    delByCond "mid1" conn mempty
    delByCond "root" conn mempty
    (fst -> (outIns' :: ["id" := Int32 :. "mid1_root_fk" := ["id" := Int32 :. Mid1Rec]])) <-
      insJSON "root" conn $ rootsIn <&> (("mid1_root_fk" =: mid1In) :.)
    (fst -> (outSel1' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    (outUps' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec], _txt) <-
      upsJSON "root" conn $ outIns' <&> \(ir :. Tagged ms) -> "mid1_root_fk" =:
        (ms <&> \(i :. flag :. x) -> i :. (not <$> flag) :. x) :. ir
    -- T.putStrLn $ "\n\n" <> txt <> "\n\n"
    (fst -> (outSel2' :: ["id" := Int32 :. "mid1_root_fk" := [Mid1Rec] :. RootRec])) <-
      selSch "root" conn qpEmpty
    pure (outIns', outSel1', outUps', outSel2')
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


prop_hier_select_one_child_list :: Pool Connection -> Property
prop_hier_select_one_child_list _pool = property success

prop_hier_select_two_child_lists_same_fk :: Pool Connection -> Property
prop_hier_select_two_child_lists_same_fk _pool = property success

prop_hier_select_child_with_parent :: Pool Connection -> Property
prop_hier_select_child_with_parent _pool = property success

prop_hier_duplicate_names_root_nested :: Pool Connection -> Property
prop_hier_duplicate_names_root_nested _pool = property success
