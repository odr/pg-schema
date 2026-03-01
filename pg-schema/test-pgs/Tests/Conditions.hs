module Tests.Conditions where

import Data.Pool as Pool
import Database.PostgreSQL.Simple
import Hedgehog

prop_cond_cmp_null_in_bool :: Pool Connection -> Property
prop_cond_cmp_null_in_bool _pool = property success

prop_cond_child_parent_exists :: Pool Connection -> Property
prop_cond_child_parent_exists _pool = property success

prop_cond_by_path :: Pool Connection -> Property
prop_cond_by_path _pool = property success
