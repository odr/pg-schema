{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Conditions where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Functor
import Data.Function
import Data.List qualified as L
import Data.Ord
import Data.Pool as Pool
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import GHC.Int
import GHC.Stack (HasCallStack)
import Hedgehog
import PgSchema.DML
import Utils


data LeafI = MkLeafI
  { leafNo :: Int32
  , value :: Double }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass GenDefault

data Mid2I = MkMid2I
  { seq :: Int32
  , kind :: Text
  , flag :: Bool
  , priority :: Int32 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass GenDefault

data Mid1I = MkMid1I
  { pos :: Int32
  , flag :: Bool
  , sortKey :: Int32
  , payload :: Maybe Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass GenDefault

data RootI = MkRootI
  { code :: Text
  , grp :: Int32
  , someEmpty :: ()
  , name :: Text }
  deriving stock (Show, Generic)
  deriving anyclass GenDefault

type InsData = "mid1_root_fk" := [Mid1I]
  :. "mid2_root_fk" := ["leaf_mid2_fk" := [LeafI] :. Mid2I]
  :. RootI

-- | Two edges in the result type that renamer maps to the same DB fk
-- ('mid1_root_fk2' -> 'mid1_root_fk'); each branch gets its own 'qPath' filter.
data RootDualMid1 = MkRootDualMid1
  { code :: Text
  , grp :: Int32
  , someEmpty :: ()
  , name :: Text
  , mid1RootFk :: [Mid1I] -- Renamer renames it to "mid1_root_fk"
  , mid1_root_fk2 :: [Mid1I] -- Renamer renames it to "mid1_root_fk"
  }
  deriving stock (Show, Generic)
  deriving anyclass GenDefault

eqRoot :: RootI -> RootI -> Bool
eqRoot r1 r2 = (r1.code, r1.grp) == (r2.code, r2.grp)

insData :: MonadIO m => Pool Connection -> PropertyT m [InsData]
insData pool = do
  rootsIn <- forAll (L.nubBy eqRoot <$> genData' RootI 1 200)
  mid1In <- forAll (genData' Mid1I 0 10)
  mid2In <- forAll (L.nubBy ((==) `on` (.seq)) <$> genData' Mid2I 0 10)
  leafIn <- forAll (L.nubBy ((==) `on` (.leafNo)) <$> genData' LeafI 0 10)
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
  L.length (L.filter rootMatches inIns) === L.length res
  where
    rootMatches (PgTag m1s :. _ :. r) =
      (r.grp > 100 || r.grp `L.elem` [0..70])
      && L.any ((<100) . (.sortKey))
        (L.take 2 $ L.sortBy (comparing (.pos)) m1s)

-- | 'qPath' / 'qWhere' on 'mid1_root_fk' vs 'mid1_root_fk2' must not be merged
-- just because both edges share one fk in the database.
prop_renamer_alias_dual_fields :: Pool Connection -> Property
prop_renamer_alias_dual_fields pool = withTests 30 $ property do
  inIns <- insData pool
  (sel :: [RootDualMid1], _) <- evalIO $ withResource pool \conn ->
    selSch "root" conn $ qRoot do
      qPath "mid1_root_fk" do
        qWhere $ "flag" =? True
      qPath "mid1_root_fk2" do
        qWhere $ "pos" >? (5 :: Int32)
  forM_ sel \root -> do
    let
      expectFk = [ m1
        | PgTag m1s :. _ :. r <- inIns
        , (r.code, r.grp) == (root.code, root.grp)
        , m1 <- m1s
        , m1.flag
        ]
      expectFk2 = [ m1
        | PgTag m1s :. _ :. r <- inIns
        , (r.code, r.grp) == (root.code, root.grp)
        , m1 <- m1s
        , m1.pos > 5
        ]
    L.sort root.mid1RootFk === L.sort expectFk
    L.sort root.mid1_root_fk2 === L.sort expectFk2

-- | Duplicate 'mid2_root_fk' in the result type: filters on the path
-- and on nested 'leaf_mid2_fk' apply per branch and may diverge.
prop_cond_by_dup_path :: Pool Connection -> Property
prop_cond_by_dup_path pool = withTests 30 $ property do
  inIns <- insData pool
  (sel :: ["mid2_root_fk" := [Mid2I] :. InsData], _) <- evalIO $ withResource pool \conn ->
    selSch "root" conn $ qRoot
      $ qPath "mid2_root_fk" do -- there are two "mid2_root_fk" branches
        qWhere $ "flag" =? True
        qPath "leaf_mid2_fk" do -- only one "leaf_mid2_fk" branch
          qWhere $ "leaf_no" >? (97::Int32)
  L.sort [m2 | (PgTag m2s :. _) <- sel, m2 <- m2s]
    === L.sort [m2 | (_ :. PgTag m2s :. _) <- inIns, (_ :. m2) <- m2s, m2.flag]
  L.sort [(m2, L.length ls) | (_ :. _ :. PgTag m2s :. _) <- sel, (PgTag ls :. m2) <- m2s]
    === L.sort [(m2, L.length $ L.filter ((>97) . (.leafNo)) ls)
      | (_ :. PgTag m2s :. _) <- inIns, (PgTag ls :. m2) <- m2s, m2.flag]
