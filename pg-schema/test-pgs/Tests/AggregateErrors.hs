{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
-- | Compile-time errors of aggregates whose Haskell type does not match
-- the SQL result type of the aggregate function.
--
-- The module is compiled with @-fdefer-type-errors@, so each ill-typed
-- definition throws 'TypeError' at runtime with the compile-time message.
module Tests.AggregateErrors where

import Control.Exception (TypeError(..), evaluate, try)
import Data.Int
import Data.List qualified as L
import Data.Text qualified as T
import Hedgehog
import PgSchema.DML
import Utils

-- | Check that rendering the SELECT for @r@ fails with a type error
-- containing all fragments.
--
-- The value is hidden behind a given equality so that the deferred error
-- is thrown only inside 'try'.
typeErrorWith :: [String] -> (forall u. u ~ () => Int) -> Property
typeErrorWith fragments x = withTests 1 $ property do
  res <- evalIO $ try @TypeError $ evaluate (x @())
  case res of
    Right _ -> do
      footnote "expected a type error, but the value was evaluated"
      failure
    Left (TypeError msg) -> do
      footnote msg
      assert $ L.all (`L.isInfixOf` msg) fragments

-- | @avg(int4)@ is @numeric@: 'Double' is rejected at compile time
-- (postgresql-simple fails at runtime with @Incompatible@).
prop_aggr_avg_int_double :: Property
prop_aggr_avg_int_double = typeErrorWith
  [ "Aggregate 'avg' over a field of type 'int4' returns SQL type 'numeric',"
  , "but Haskell type Double is not compatible with it."
  , "Use Scientific or Rational instead."
  , "DB Field name: \"i4\"" ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("i4" := Aggr AAvg (Maybe Double)) qpEmpty)

-- | @sum(int8)@ is @numeric@, not @int8@.
prop_aggr_sum_int8_int64 :: Property
prop_aggr_sum_int8_int64 = typeErrorWith
  [ "Aggregate 'sum' over a field of type 'int8' returns SQL type 'numeric',"
  , "but Haskell type Int64 is not compatible with it." ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("grp" := Int32 :. "i8" := Aggr' ASum Int64) qpEmpty)

-- | @sum(int4)@ is @int8@, so 'Int32' is not enough.
prop_aggr_sum_int4_int32 :: Property
prop_aggr_sum_int4_int32 = typeErrorWith
  [ "Aggregate 'sum' over a field of type 'int4' returns SQL type 'int8',"
  , "but Haskell type Int32 is not compatible with it."
  , "Use Int64, Scientific or Rational instead." ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("i4" := Aggr ASum (Maybe Int32)) qpEmpty)

-- | @avg(numeric)@ is @numeric@.
prop_aggr_avg_numeric_double :: Property
prop_aggr_avg_numeric_double = typeErrorWith
  [ "Aggregate 'avg' over a field of type 'numeric' returns SQL type 'numeric',"
  , "but Haskell type Double is not compatible with it." ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("n" := Aggr AAvg (Maybe Double)) qpEmpty)

-- | @sum@ over a text field.
prop_aggr_sum_text :: Property
prop_aggr_sum_text = typeErrorWith
  [ "Aggregate 'sum' is not supported for fields of type 'text'" ]
  (T.length $ fst $ selectText (AnnSch "root")
    @("name" := Aggr ASum (Maybe Int64)) qpEmpty)

-- | 'Aggr' (without GROUP BY) can be NULL, so it requires 'Maybe'.
prop_aggr_without_maybe :: Property
prop_aggr_without_maybe = typeErrorWith
  [ "'Aggr' (except 'Aggr ACount') requires 'Maybe'" ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("i2" := Aggr ASum Int64) qpEmpty)

-- | 'Aggr'' over a nullable field requires 'Maybe'.
prop_aggr'_nullable_without_maybe :: Property
prop_aggr'_nullable_without_maybe = typeErrorWith
  [ "You have to use Maybe in 'Aggr'' for nullable fields" ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("grp" := Int32 :. "f8" := Aggr' AAvg Double) qpEmpty)

-- | 'Aggr'' over a mandatory field must not be 'Maybe'.
prop_aggr'_mandatory_with_maybe :: Property
prop_aggr'_mandatory_with_maybe = typeErrorWith
  [ "You can't use Maybe in 'Aggr'' for mandatory fields" ]
  (T.length $ fst $ selectText (AnnSch "aggr_nums")
    @("grp" := Int32 :. "f4" := Aggr' AMax (Maybe Double)) qpEmpty)
