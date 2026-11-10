{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
-- | Compile-time errors of 'named' and 'rowsOf'.
--
-- The module is compiled with @-fdefer-type-errors@, so each ill-typed
-- definition throws 'TypeError' at runtime with the compile-time message.
module Tests.NamedErrors where

import Control.Exception (TypeError(..), evaluate, try)
import Data.List qualified as L
import Hedgehog
import PgSchema.DML

-- | Check that evaluating a value fails with a type error containing all fragments.
--
-- The value is hidden behind a given equality: deferred coercion evidence of
-- 'named' is strict, so without it the error is thrown as soon as the
-- property itself is evaluated (outside 'try').
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

prop_named_empty :: Property
prop_named_empty = typeErrorWith
  [ "named: the list of field names is empty.", "The row: Bool" ]
  (L.length $ named [] [True])

prop_named_fewer_names :: Property
prop_named_fewer_names = typeErrorWith
  [ "named: there are fewer field names than fields in the row."
  , "The last name \"b\" is given to more than one field: Bool :. Char" ]
  (L.length $ named ["a", "b"] [(1 :: Int) :. True :. 'c'])

prop_named_more_names :: Property
prop_named_more_names = typeErrorWith
  [ "named: there are more field names than fields in the row."
  , "Field \"b\" gets the last field Bool"
  , "Names without fields: [\"c\", \"d\"]" ]
  (L.length $ named ["a", "b", "c", "d"] [(1 :: Int) :. True])

-- | The same error as for 'Generic'-based rows in DML.
prop_rows_of_not_product :: Property
prop_rows_of_not_product = typeErrorWith
  [ "Only Product types with fields are supported in pg-schema."
  , "But I've got " ]
  (L.length $ rowsOf @Ordering [])
