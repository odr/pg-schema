module Tests.Hierarchy where

import Data.Pool as Pool
import Database.PostgreSQL.Simple
import Hedgehog

prop_hier_insert_simple_fk :: Pool Connection -> Property
prop_hier_insert_simple_fk _pool = property success

prop_hier_insert_composite_fk :: Pool Connection -> Property
prop_hier_insert_composite_fk _pool = property success

prop_hier_select_one_child_list :: Pool Connection -> Property
prop_hier_select_one_child_list _pool = property success

prop_hier_select_two_child_lists_same_fk :: Pool Connection -> Property
prop_hier_select_two_child_lists_same_fk _pool = property success

prop_hier_select_child_with_parent :: Pool Connection -> Property
prop_hier_select_child_with_parent _pool = property success

prop_hier_duplicate_names_root_nested :: Pool Connection -> Property
prop_hier_duplicate_names_root_nested _pool = property success
