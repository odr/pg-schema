{-# LANGUAGE BlockArguments #-}
module Tests.Aggregates where

import Control.Monad
import Data.List qualified as L
import Data.Pool as Pool
import Data.Scientific
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


type AggrNumsRow =
  "grp" := Int32 :. "i2" := Int16 :. "i4" := Maybe Int32 :. "i8" := Int64
  :. "n" := Maybe Scientific :. "f4" := Double :. "f8" := Maybe Double

aggrNumsRows :: [AggrNumsRow]
aggrNumsRows =
  [ "grp" =: 1 :. "i2" =: 1 :. "i4" =: Just 10 :. "i8" =: 100
    :. "n" =: Just 1.5 :. "f4" =: 0.5 :. "f8" =: Just 0.25
  , "grp" =: 1 :. "i2" =: 2 :. "i4" =: Nothing :. "i8" =: 200
    :. "n" =: Just 2.5 :. "f4" =: 1.5 :. "f8" =: Nothing
  , "grp" =: 2 :. "i2" =: 3 :. "i4" =: Just 30 :. "i8" =: 300
    :. "n" =: Nothing :. "f4" =: 4 :. "f8" =: Just 1 ]

-- | Insert 'aggrNumsRows' into the empty @aggr_nums@ table and select @r@.
selectAggrNums
  :: forall r. Selectable (AnnSch "aggr_nums") r
  => Pool Connection -> IO [r]
selectAggrNums pool = withPool pool \conn -> do
  void $ delByCond "aggr_nums" conn mempty
  void $ insSch_ "aggr_nums" conn aggrNumsRows
  fst <$> selSch "aggr_nums" conn qpEmpty

-- | Haskell types of @sum@ follow its SQL result type:
-- @sum(int2 | int4)@ is @int8@, @sum(int8 | numeric)@ is @numeric@,
-- @sum(float4)@ is @float4@, @sum(float8)@ is @float8@.
type SumsRow =
  "cnt" := Aggr ACount Int64
  :. "i2" := Aggr ASum (Maybe Int64)
  :. "i4" := Aggr ASum (Maybe Int64)
  :. "i8" := Aggr ASum (Maybe Scientific)
  :. "n" := Aggr ASum (Maybe Rational)
  :. "f4" := Aggr ASum (Maybe Double)
  :. "f8" := Aggr ASum (Maybe Double)

prop_aggr_sum_result_types :: Pool Connection -> Property
prop_aggr_sum_result_types pool = withTests 1 $ property do
  res <- evalIO $ selectAggrNums @SumsRow pool
  res ===
    [ "cnt" =: Aggr 3 :. "i2" =: Aggr (Just 6) :. "i4" =: Aggr (Just 40)
      :. "i8" =: Aggr (Just 600) :. "n" =: Aggr (Just 4) :. "f4" =: Aggr (Just 6)
      :. "f8" =: Aggr (Just 1.25) ]

-- | Haskell types of @avg@ follow its SQL result type:
-- @avg(int2 | int4 | int8 | numeric)@ is @numeric@, @avg(float4 | float8)@ is @float8@.
type AvgsRow =
  "i2" := Aggr AAvg (Maybe Rational)
  :. "i4" := Aggr AAvg (Maybe Scientific)
  :. "i8" := Aggr AAvg (Maybe Scientific)
  :. "n" := Aggr AAvg (Maybe Scientific)
  :. "f4" := Aggr AAvg (Maybe Double)
  :. "f8" := Aggr AAvg (Maybe Double)

prop_aggr_avg_result_types :: Pool Connection -> Property
prop_aggr_avg_result_types pool = withTests 1 $ property do
  res <- evalIO $ selectAggrNums @AvgsRow pool
  res ===
    [ "i2" =: Aggr (Just 2) :. "i4" =: Aggr (Just 20) :. "i8" =: Aggr (Just 200)
      :. "n" =: Aggr (Just 2) :. "f4" =: Aggr (Just 2) :. "f8" =: Aggr (Just 0.625) ]

-- | 'Aggr'' with GROUP BY: 'Maybe' exactly for nullable fields;
-- @min@ / @max@ keep the field type.
type GroupedRow =
  "grp" := Int32
  :. "i2" := Aggr' ASum Int64
  :. "i4" := Aggr' ASum (Maybe Int64)
  :. "i8" := Aggr' AAvg Scientific
  :. "n" := Aggr' AMax (Maybe Scientific)
  :. "f4" := Aggr' AMin Double
  :. "f8" := Aggr' AAvg (Maybe Double)

prop_aggr_grouped_result_types :: Pool Connection -> Property
prop_aggr_grouped_result_types pool = withTests 1 $ property do
  res <- evalIO $ selectAggrNums @GroupedRow pool
  L.sortOn (\(PgTag g :. _) -> g) res ===
    [ "grp" =: 1 :. "i2" =: Aggr' 3 :. "i4" =: Aggr' (Just 10)
      :. "i8" =: Aggr' 150 :. "n" =: Aggr' (Just 2.5) :. "f4" =: Aggr' 0.5
      :. "f8" =: Aggr' (Just 0.25)
    , "grp" =: 2 :. "i2" =: Aggr' 3 :. "i4" =: Aggr' (Just 30)
      :. "i8" =: Aggr' 300 :. "n" =: Aggr' Nothing :. "f4" =: Aggr' 4
      :. "f8" =: Aggr' (Just 1) ]
