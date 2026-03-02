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

prop_not_implemented :: Property
prop_not_implemented = property $ fail "not implemented"

main :: IO ()
main = do
  connStr <- maybe "dbname=schema_test" BS.pack <$> lookupEnv "PG_CONN"
  pool <- newPool $ defaultPoolConfig (connectPostgreSQL connStr) close 10 10
  defaultMain $ testGroup "DB Tests"
    [ testGroup "Base converts (test_schema)"
      [ testProperty "Work with base types" $ prop_base_converts pool
      -- , testProperty "Work with array of base types" $ prop_base_arr_converts pool
      -- , testProperty "Work with extra types (bytea, jsonb, enums, uuid)" $ prop_ext_converts pool
      -- , testProperty "Work with array of extra types" $ prop_ext_arr_converts pool
      ]
    , testGroup "Hierarchy (test_dml)"
      [ testProperty "Insert root then children (simple FK)" $ prop_hier_insert_simple_fk pool
      , testProperty "Insert root then children (composite FK)" $ prop_hier_insert_composite_fk pool
      -- , testProperty "Select root with one child list (RFToHere)" prop_not_implemented
      -- , testProperty "Select root with two child lists by same FK target (dim_a, dim_b)" prop_not_implemented
      -- , testProperty "Select child with parent (RFFromHere)" prop_not_implemented
      -- , testProperty "Duplicate field names in root and nested structure" prop_not_implemented
      ]
    -- , testGroup "Conditions (test_dml)"
    --   [ testProperty "Cond: Cmp, Null, In, BoolOp (and/or)" prop_not_implemented
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
