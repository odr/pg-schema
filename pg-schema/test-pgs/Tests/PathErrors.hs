{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
-- | Compile-time errors of 'qPath' / 'qPathFromHere' / 'qPathToHere'.
--
-- The module is compiled with @-fdefer-type-errors@, so each ill-typed
-- definition throws 'TypeError' at runtime with the compile-time message.
-- Each path ends with 'qWhere' so that the (deferred) path evidence is
-- used when the condition's path is evaluated.
module Tests.PathErrors where

import Control.Exception (TypeError(..), evaluate, try)
import Data.List qualified as L
import Hedgehog
import PgSchema.DML
import Utils

-- | Check that evaluating paths of conditions fails with a type error
-- containing all fragments.
typeErrorWith :: [String] -> QueryParamAnn (AnnSch tn) -> Property
typeErrorWith fragments qp = withTests 1 $ property do
  res <- evalIO $ try @TypeError $ evaluate
    $ sum [ L.length (show p) | CondWithPath p _ <- qp.qpConds ]
  case res of
    Right _ -> do
      footnote "expected a type error, but the value was evaluated"
      failure
    Left (TypeError msg) -> do
      footnote msg
      assert $ L.all (`L.isInfixOf` msg) fragments
      assert $ not ("self-reference" `L.isInfixOf` msg)

relNotDefined :: String -> String -> [String]
relNotDefined tab rel =
  [ "for table NameNS \"test_pgs\" \"" <> tab <> "\""
  , "relation \"" <> rel <> "\""
  , " is not defined." ]

-- | Unknown relation in 'qPath' (used to report a self-reference error).
prop_qpath_unknown_rel :: Property
prop_qpath_unknown_rel = typeErrorWith
  ( relNotDefined "root" "mid1_rot_fk"
  <> [ "root_dim_a_fk, root_dim_b_fk"
     , "arrays_root_fk, mid1_root_fk, mid2_root_fk" ] )
  (qRoot $ qPath "mid1_rot_fk" here :: QueryParamAnn (AnnSch "root"))

-- | Unknown relation in 'qPath' after applying the renamer.
prop_qpath_unknown_rel_renamed :: Property
prop_qpath_unknown_rel_renamed = typeErrorWith
  ( relNotDefined "root" "mid1_rot_fk"
  <> [ "(\"mid1RotFk\" after applying the renamer)" ] )
  (qRoot $ qPath "mid1RotFk" here :: QueryParamAnn (AnnSch "root"))

-- | Unknown relation in a nested 'qPath'.
prop_qpath_unknown_rel_nested :: Property
prop_qpath_unknown_rel_nested = typeErrorWith
  (relNotDefined "mid2" "leaf_mid_fk" <> ["mid2_root_fk", "leaf_mid2_fk"])
  ( qRoot $ qPath "mid2_root_fk" $ qPath "leaf_mid_fk" here
    :: QueryParamAnn (AnnSch "root") )

-- | Unknown relation in 'qPathFromHere'.
prop_qpath_from_here_unknown_rel :: Property
prop_qpath_from_here_unknown_rel = typeErrorWith
  (relNotDefined "mid1" "mid1_rot_fk" <> ["mid1_root_fk", "<none>"])
  (qRoot $ qPathFromHere "mid1_rot_fk" here :: QueryParamAnn (AnnSch "mid1"))

-- | Unknown relation in 'qPathToHere'.
prop_qpath_to_here_unknown_rel :: Property
prop_qpath_to_here_unknown_rel = typeErrorWith
  (relNotDefined "root" "mid1_rot_fk")
  (qRoot $ qPathToHere "mid1_rot_fk" here :: QueryParamAnn (AnnSch "root"))

-- | Existing relation used in the wrong direction.
prop_qpath_from_here_wrong_dir :: Property
prop_qpath_from_here_wrong_dir = typeErrorWith
  [ "Relation is not available in from-here direction.", "\"mid1_root_fk\"" ]
  (qRoot $ qPathFromHere "mid1_root_fk" here :: QueryParamAnn (AnnSch "root"))

-- | Existing relation used in the wrong direction.
prop_qpath_to_here_wrong_dir :: Property
prop_qpath_to_here_wrong_dir = typeErrorWith
  [ "Relation is not available in to-here direction.", "\"mid1_root_fk\"" ]
  (qRoot $ qPathToHere "mid1_root_fk" here :: QueryParamAnn (AnnSch "mid1"))

here :: forall ren sch t path. MonadQP ren sch t path
here = qWhere mempty
