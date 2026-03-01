module Tests.Path where

import Data.Pool as Pool
import Database.PostgreSQL.Simple
import Hedgehog

prop_path_distinct_root :: Pool Connection -> Property
prop_path_distinct_root _pool = property success

prop_path_distinct_parent_path :: Pool Connection -> Property
prop_path_distinct_parent_path _pool = property success

prop_path_order :: Pool Connection -> Property
prop_path_order _pool = property success

prop_path_group_by :: Pool Connection -> Property
prop_path_group_by _pool = property success
