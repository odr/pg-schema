{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Rows built with 'named' (names given once for the whole list)
-- and with 'rowsOf' / 'plain' (names taken from the type).
module Tests.Named where

import Control.Monad
import Data.CaseInsensitive
import Data.Int
import Data.List qualified as L
import Data.Pool as Pool
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import Hedgehog as H
import Prelude as P
import PgSchema.DML
import Sch
import Utils


type ExtRow = "ccolor" := Maybe (EnumPGS "color") :. "ccitext" := Maybe (CI Text)

-- | Enum constructors, 'Nothing' etc. are used as is, without 'PgTag'.
-- Result must be the same as for rows built with '(=:)'.
prop_named_insert :: Pool Connection -> Property
prop_named_insert pool = withTests 1 $ property do
  let
    expected :: [ExtRow]
    expected =
      [ "ccolor" =: Just Color_red :. "ccitext" =: Just "A"
      , "ccolor" =: Just Color_blue :. "ccitext" =: Nothing
      , "ccolor" =: Nothing :. "ccitext" =: Just "c" ]
  (outIns, outSel) <- evalIO $ withPool pool \conn -> do
    void $ delByCond "ext_converts" conn mempty
    (outIns', _) <- insSch "ext_converts" conn $ named ["ccolor", "ccitext"]
      [ Just (Color_red :: EnumPGS "color") :. Just ("A" :: CI Text)
      , Just Color_blue :. Nothing
      , Nothing :. Just "c" ]
    (outSel', _) <- selSch "ext_converts" conn qpEmpty
    pure (outIns', outSel')
  outIns === expected
  L.sort outSel === L.sort expected

-- | Nested rows: a list of children is just another 'named' call.
prop_named_insert_tree :: Pool Connection -> Property
prop_named_insert_tree pool = withTests 1 $ property do
  let
    mids pos0 = named ["pos", "flag", "sortKey"]
      [ pos0 :. True :. 1, (pos0 + 1 :: Int32) :. False :. (2 :: Int32) ]
  (outIns, outSel) <- evalIO $ withPool pool \conn -> do
    void $ delByCond "mid1" conn mempty
    void $ delByCond "root" conn mempty
    (outIns' :: ["code" := Text :. "mid1RootFk" := ["pos" := Int32]], _) <-
      insJSON "root" conn $ named ["code", "grp", "name", "mid1RootFk"]
        [ ("r1" :: Text) :. (1 :: Int32) :. ("first" :: Text) :. mids 10
        , "r2" :. 2 :. "second" :. mids 20 ]
    (outSel', _) <- selSch "root" conn qpEmpty
    pure (outIns', outSel')
  outIns ===
    [ "code" =: "r1" :. "mid1RootFk" =: ["pos" =: 10, "pos" =: 11]
    , "code" =: "r2" :. "mid1RootFk" =: ["pos" =: 20, "pos" =: 21] ]
  L.sort outSel ===
    [ "code" =: ("r1" :: Text) :. "grp" =: (1 :: Int32)
      :. "mid1RootFk" =: L.sort ["pos" =: (10 :: Int32) :. "flag" =: True, "pos" =: 11 :. "flag" =: False]
    , "code" =: "r2" :. "grp" =: 2
      :. "mid1RootFk" =: L.sort ["pos" =: 20 :. "flag" =: True, "pos" =: 21 :. "flag" =: False] ]

type RootN = "code" := Text :. "grp" := Int32 :. "name" := Text

type Mid1N = "pos" := Int32 :. "flag" := Bool :. "sortKey" := Int32

-- | Generic record as a part of a row: its fields are flattened.
data RootG = RootG { code :: Text, grp :: Int32 }
  deriving (Eq, Ord, Show, Generic)

-- | 'rowsOf': names and types are taken from the type; no parentheses are
-- needed for groups (RootN), Generic records are flattened, children are plain.
-- 'plain': selected rows (child with parent) as plain values.
prop_rows_of_tree :: Pool Connection -> Property
prop_rows_of_tree pool = withTests 1 $ property do
  (outIns, outSel) <- evalIO $ withPool pool \conn -> do
    void $ delByCond "mid1" conn mempty
    void $ delByCond "root" conn mempty
    (outIns', _) <- insJSON "root" conn $ rowsOf (type (RootN :. "mid1RootFk" := [Mid1N]))
      [ "r1" :. 1 :. "first" :. [10 :. True :. 1, 11 :. False :. 2]
      , "r2" :. 2 :. "second" :. [20 :. True :. 3] ]
    (outSel', _) <- selSch "mid1" conn qpEmpty
    pure (outIns' :: [RootG :. "mid1RootFk" := ["pos" := Int32]], outSel')
  plain outIns ===
    [ "r1" :. 1 :. [10, 11]
    , "r2" :. 2 :. [20] ]
  L.sort (plain (outSel :: [Mid1N :. "mid1RootFk" := (RootG :. "name" := Text)])) ===
    [ 10 :. True :. 1 :. ("r1" :. 1 :. "first")
    , 11 :. False :. 2 :. ("r1" :. 1 :. "first")
    , 20 :. True :. 3 :. ("r2" :. 2 :. "second") ]

-- | 'rowsOf' with a Generic record (in the inserted and in the expected rows).
-- A type written in place needs the @type@ keyword: otherwise '(:=)' and
-- 'RootG' are taken as data constructors.
prop_rows_of_generic :: Pool Connection -> Property
prop_rows_of_generic pool = withTests 1 $ property do
  outSel <- evalIO $ withPool pool \conn -> do
    void $ delByCond "mid1" conn mempty
    void $ delByCond "root" conn mempty
    void $ insJSON_ "root" conn $ rowsOf (type (RootG :. "name" := Text :. "mid1RootFk" := [Mid1N]))
      [ "g1" :. 1 :. "first" :. [1 :. True :. 1] ]
    fst <$> selSch "root" conn qpEmpty
  outSel === rowsOf (type (RootG :. "mid1RootFk" := [Mid1N])) [ "g1" :. 1 :. [1 :. True :. 1] ]
