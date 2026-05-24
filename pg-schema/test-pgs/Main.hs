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
import Tests.InsertJSONTransaction
import Tests.UpsertUniqueKey
import Tests.KeyedDML
import Tests.Conditions
import Tests.Aggregates

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
      , testProperty "InsertJSON: json/jsonb string values use -> operator" $ prop_ext_converts_json_string_via_upsertjson pool
      ]
    , testGroup "Hierarchy (test_dml)"
      [ testProperty "Insert/Upsert/Select root with children (simple FK)" $ prop_hier_insert_simple_fk pool
      , testProperty "Insert/Upsert/Select root with children (composite FK)" $ prop_hier_insert_composite_fk pool
      , testProperty "Select child with parent (RFFromHere)" $ prop_hier_select_child_with_parent pool
      , testProperty "Duplicate field names in root and nested structure" $ prop_hier_duplicate_names_root_nested pool
      -- , testProperty "Optional parent FK on root (dim_a_id)" $
      --     prop_hier_insert_optional_parent_dim_a pool
      ]
    , testGroup "InsertJSON transactions (test_dml)"
      [ testProperty "Standalone call commits" $
          prop_insertJSON_standalone_commits pool
      , testProperty "Outer transaction is not committed" $
          prop_insertJSON_respects_outer_transaction pool
      , testProperty "BEGIN without prior writes rolls back" $
          prop_insertJSON_begin_without_prior_writes pool
      , testProperty "Failure rolls back standalone transaction" $
          prop_insertJSON_rolls_back_on_failure pool
      ]
    , testGroup "Keyed DML (test_dml)"
      [ testProperty "upsertByKey insert then key patch" $
          prop_upsert_by_key_insert_then_update pool
      , testProperty "upsertByKey composite unique" $
          prop_upsert_by_key_composite_unique pool
      , testProperty "upsertByKey always INSERT ON CONFLICT SQL" $
          prop_upsert_by_key_never_pure_update pool
      , testProperty "updateByKey found" $ prop_update_by_key_found pool
      , testProperty "updateByKey not found" $
          prop_update_by_key_not_found pool
      , testProperty "updateByKey never inserts" $
          prop_update_by_key_never_inserts pool
      , testProperty "updateJSON bad root id" $
          prop_update_json_not_found_returns_nothing pool
      , testProperty "updateJSON found" $ prop_update_json_found pool
      , testProperty "upsertJSON returning positions" $
          prop_upsert_json_returning_positions pool
      , testProperty "upsertJSON full row bare returning" $
          prop_upsert_json_full_row_bare_returning pool
      , testProperty "updateJSON child key miss" $
          prop_update_json_child_maybe pool
      ]
    , testGroup "Upsert by unique key (test_dml)"
      [ testProperty "upsertJSON on composite unique without PK in payload" $
          prop_upsert_json_by_composite_unique pool
      , testProperty "upsertJSON on nullable unique without NOT NULL UK" $
          prop_upsert_json_by_nullable_unique pool
      , testProperty "upsertJSON UPDATE by nullable unique key" $
          prop_upsert_json_update_by_nullable_unique pool
      , testProperty "upsertJSON null in nullable key inserts again" $
          prop_upsert_json_nullable_key_null_inserts_twice pool
      ]
    , testGroup "Query (test_dml)"
      [ testProperty "'Simple' queries" $ prop_cond_query pool
      , testProperty "Conditions by duplicated path" $ prop_cond_by_dup_path pool
      ]
    , testGroup "Aggregates (test_dml)"
      [ testProperty "Aggr' on plain column is not in GROUP BY" $
          prop_selectText_aggr_plain_not_in_group_by pool
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
