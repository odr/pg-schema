module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Pool as Pool
import Database.PostgreSQL.Simple
import Hedgehog
import System.Environment
import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.BaseConverts
import Tests.Hierarchy
import Tests.Conditions

prop_not_implemented :: Property
prop_not_implemented = property $ fail "not implemented"

main :: IO ()
main = do
  connStr <- maybe "dbname=schema_test" BS.pack <$> lookupEnv "PG_CONN"
  pool <- newPool $ defaultPoolConfig (connectPostgreSQL connStr) close 10 10
  defaultMain $ testGroup "DB Tests"
    [ testGroup "Base converts (test_schema)"
      [ testProperty "Base types" $ prop_base_converts pool
      , testProperty "Array of base types" $ prop_base_arr_converts pool
      , testProperty "Extra types (bytea, jsonb, enums, uuid)" $ prop_ext_converts pool
      , testProperty "Array of extra types" $ prop_ext_arr_converts pool
      ]
    , testGroup "Hierarchy (test_dml)"
      [ testProperty "Insert/Upsert/Select root with children (simple FK)" $ prop_hier_insert_simple_fk pool
      , testProperty "Insert/Upsert/Select root with children (composite FK)" $ prop_hier_insert_composite_fk pool
      , testProperty "Select child with parent (RFFromHere)" $ prop_hier_select_child_with_parent pool
      , testProperty "Duplicate field names in root and nested structure" $ prop_hier_duplicate_names_root_nested pool
      ]
    , testGroup "Query (test_dml)"
      [ testProperty "'Simple' queries" $ prop_cond_query pool
      , testProperty "Conditions by duplicated path" $ prop_cond_by_dup_path pool
      ]
    --   , testProperty "Cond: Child / Parent exists" prop_not_implemented
    --   , testProperty "Conditions by path (root vs nested path)" prop_not_implemented
    --   ]
    -- , testGroup "Distinct / Order / Group by path (test_dml)"
    --   [ testProperty "Distinct on root fields" prop_not_implemented
    --   , testProperty "Distinct on parent path fields (join order vs qPath order)" prop_not_implemented
    --   , testProperty "Order by root; order by path" prop_not_implemented
    --   , testProperty "Group by path fields" prop_not_implemented
    --   ]
    -- , testGroup "Aggregates and duplicate names (test_dml)"
    --   [ testProperty "Multiple aggregate fields (max, min) in one select" prop_not_implemented
    --   , testProperty "Duplicate names: several group/agg fields in nested structure" prop_not_implemented
    --   ]
    ]
