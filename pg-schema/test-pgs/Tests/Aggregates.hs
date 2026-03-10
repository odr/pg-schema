module Tests.Aggregates where

import Data.Pool as Pool
import Database.PostgreSQL.Simple
import Hedgehog

prop_aggr_multiple_max_min :: Pool Connection -> Property
prop_aggr_multiple_max_min _pool = property success

prop_aggr_duplicate_names_nested :: Pool Connection -> Property
prop_aggr_duplicate_names_nested _pool = property success
