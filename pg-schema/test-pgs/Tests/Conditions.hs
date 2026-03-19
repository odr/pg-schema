{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Conditions where

import Control.Monad.IO.Class
import Data.Functor
import Data.Function
import Data.List qualified as L
import Data.Ord
import Data.Pool as Pool
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Generics
import GHC.Int
import GHC.Stack (HasCallStack)
import Hedgehog
import PgSchema.DML
import Utils


data RootI = MkRootI
  { code :: Text
  , grp :: Int32
  , someEmpty :: ()
  , name :: Text }
  deriving (Show, Generic)
  deriving anyclass GenDefault

data Mid1I = MkMid1I
  { pos :: Int32
  , flag :: Bool
  , sort_key :: Int32
  , payload :: Maybe Text }
  deriving (Show, Generic)
  deriving anyclass GenDefault

data Mid2I = MkMid2I
  { seq :: Int32
  , kind :: Text
  , flag :: Bool
  , priority :: Int32 }
  deriving (Show, Generic, Eq, Ord)
  deriving anyclass GenDefault

data LeafI = MkLeafI
  { leaf_no :: Int32
  , value :: Double }
  deriving (Generic, Eq, Ord, Show)
  deriving anyclass GenDefault

eqRoot :: RootI -> RootI -> Bool
eqRoot r1 r2 = (r1.code, r1.grp) == (r2.code, r2.grp)

type InsData = "mid1_root_fk" := [Mid1I]
  :. "mid2_root_fk" := ["leaf_mid2_fk" := [LeafI] :. Mid2I]
  :. RootI

insData :: MonadIO m => Pool Connection -> PropertyT m [InsData]
insData pool = do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootI 1 200)
  mid1In <- forAll (genData' Mid1I 0 10)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genData' Mid2I 0 10)
  leafIn <- forAll (L.nubBy ((==) `on` (.leaf_no)) <$> genData' LeafI 0 10)
  let
    inIns = rootsIn <&> \r -> "mid1_root_fk" =: mid1In
      :. "mid2_root_fk" =: (mid2In <&> \m2 -> "leaf_mid2_fk" =: leafIn :. m2)
      :. r
  evalIO $ withResource pool \conn -> do
    delByCond "leaf" conn mempty
    delByCond "mid1" conn mempty
    delByCond "mid2" conn mempty
    delByCond "root" conn mempty
    void $ insJSON_ "root" conn inIns
  pure inIns

prop_cond_query :: Pool Connection -> Property
prop_cond_query pool = withTests 30 $ property do
  inIns <- insData pool
  (res :: [InsData], _) <- evalIO $ withResource pool \conn ->
    selSch "root" conn $ qRoot do
      qWhere $ ("grp" >? (100::Int32) ||| pinArr "grp" ([0..70]::[Int32]))
        &&& pchild (TS "mid1_root_fk")
          defTabParam { order = [ascf "pos"], lo = LO (Just 2) Nothing }
          ("sort_key" <? (100::Int32))
      qPath "mid1_root_fk" do
        qDistinctOn [ascf "root_id"]
        qWhere $ pnull "payload"
      qPath "mid2_root_fk" do
        qPath "leaf_mid2_fk" do
          qWhere $ pparent (TS "leaf_mid2_fk")
            $ pparent (TS "mid2_root_fk") $ "grp" >? (0::Int32)
        qOrderBy [descf "kind"]
  L.length (L.filter (\(PgTag m1s :. _ :. r) ->
    (r.grp > 100 || r.grp `L.elem` [0..70])
    && L.any ((<100) . (.sort_key)) (L.take 2 $ L.sortBy (comparing (.pos)) m1s)
    )
    inIns) === L.length res

type OutData = "mid2_root_fk" := [Mid2I] :. InsData

-- We can have similar pathes in select.
-- Params are applied for all branches with this path
prop_cond_by_dup_path :: Pool Connection -> Property
prop_cond_by_dup_path pool = withTests 30 $ property do
  inIns <- insData pool
  (sel :: [OutData], _) <- evalIO $ withResource pool \conn ->
    selSch "root" conn $ qRoot
      $ qPath "mid2_root_fk" do
        qWhere $ "flag" =? True
        qPath "leaf_mid2_fk" do
          qWhere $ "leaf_no" >? (100::Int32)
  L.sort [m2 | (PgTag m2s :. _) <- sel, m2 <- m2s]
    === L.sort [m2 | (_ :. PgTag m2s :. _) <- inIns, (_ :. m2) <- m2s, m2.flag]
  L.sort [(m2, L.length ls) | (_ :. _ :. PgTag m2s :. _) <- sel, (PgTag ls :. m2) <- m2s]
    === L.sort [(m2, L.length $ L.filter ((>100) . (.leaf_no)) ls)
      | (_ :. PgTag m2s :. _) <- inIns, (PgTag ls :. m2) <- m2s, m2.flag]
