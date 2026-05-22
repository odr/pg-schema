{-# LANGUAGE BlockArguments #-}
module Tests.Aggregates where

import Data.Pool as Pool
import Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import GHC.Int
import Hedgehog
import PgSchema.DML
import Utils


type RootGroupSel =
  "grp" := Int32
  :. "cnt" := Aggr' ACount Int64
  :. "name" := Aggr' AMin Text


-- | 'Aggr'' on a plain column must become SQL aggregate, not GROUP BY key.
prop_selectText_aggr_plain_not_in_group_by :: Pool Connection -> Property
prop_selectText_aggr_plain_not_in_group_by _pool =
  property do
    let (sql, _) = selectText (AnnSch "root") @RootGroupSel qpEmpty
    T.isInfixOf "count(*)" sql === True
    T.isInfixOf "min(t0.name)" sql === True
    T.isInfixOf "group by grp" sql === True
    T.isInfixOf ",name" sql === False
