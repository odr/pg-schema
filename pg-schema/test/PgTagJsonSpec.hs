-- | JSON encode/decode checks for `PgTag ann r` (replaces legacy HList doctest tests).
module PgTagJsonSpec (runTests) where

import Control.Monad (unless)
import Data.Aeson (Result (..), decode, encode, fromJSON, toJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text)
import GHC.Int (Int16)
import PgSchema.DML
import PgSchema.Schema.Catalog (PGC, PgCatalog)
import PgSchema.Schema.Info (PgClassShort (..), PgEnum (..), PgRelation (..))

data RenSnake :: Renamer

type instance Apply RenSnake s = CamelToSnake s

type AnnCons = 'Ann RenSnake PgCatalog 3 (PGC "pg_constraint")

type AnnEnum = 'Ann RenSnake PgCatalog 3 (PGC "pg_enum")

runTests :: IO ()
runTests = do
  pgEnumEncodeShape
  pgEnumDecodeRoundTrip
  pgEnumMissingKeyRejected
  pgEnumExtraKeysIgnored
  pgConstraintJsonRoundTrip
  pgConstraintDuplicateFieldRoundTrip
  putStrLn "PgTag JSON (Ann / PgCatalog / CamelToSnake) checks passed."

-- | Stable field order follows Generics: @enumlabel@ then @enumsortorder@.
pgEnumEncodeShape :: IO ()
pgEnumEncodeShape = do
  let
    val :: PgTag AnnEnum PgEnum
    val = PgTag PgEnum { enumlabel = "lbl", enumsortorder = 42 :: Double }
    enc = encode val
  unless
    ( enc
        == pack "{\"enumlabel\":\"lbl\",\"enumsortorder\":42}"
        || enc == pack "{\"enumlabel\":\"lbl\",\"enumsortorder\":42.0}"
    )
    $ error $ "PgEnum encode: got " ++ show enc

pgEnumDecodeRoundTrip :: IO ()
pgEnumDecodeRoundTrip = do
  let
    val :: PgTag AnnEnum PgEnum
    val = PgTag PgEnum { enumlabel = "a", enumsortorder = 1.5 }
  case decode (encode val) of
    Nothing -> error "PgEnum decode: expected Just"
    Just v ->
      unless (v == val)
        $ error $ "PgEnum decode: got " ++ show v

pgEnumMissingKeyRejected :: IO ()
pgEnumMissingKeyRejected =
  case decode @(PgTag AnnEnum PgEnum) (pack "{\"enumsortorder\":1}") of
    Just _ -> error "PgEnum: expected decode failure for missing enumlabel"
    Nothing -> pure ()

pgEnumExtraKeysIgnored :: IO ()
pgEnumExtraKeysIgnored = do
  let
    val :: PgTag AnnEnum PgEnum
    val = PgTag PgEnum { enumlabel = "x", enumsortorder = 1 :: Double }
  case decode (pack "{\"enumlabel\":\"x\",\"enumsortorder\":1,\"extra\":99}") of
    Nothing -> error "PgEnum extra keys: expected Just"
    Just v ->
      unless (v == val)
        $ error $ "PgEnum extra keys: got " ++ show v

pgConstraintJsonRoundTrip :: IO ()
pgConstraintJsonRoundTrip = do
  let
    rel :: PgRelation
    rel =
      PgRelation
        { constraint__namespace = "nspname" =: ("a" :: Text)
        , conname = "b"
        , constraint__class = PgClassShort ("nspname" =: ("c" :: Text)) "d"
        , constraint__fclass = PgClassShort ("nspname" =: ("e" :: Text)) "f"
        , conkey = pgArr' [1 :: Int16, 2]
        , confkey = pgArr' []
        }
    tagged :: PgTag AnnCons PgRelation
    tagged = PgTag rel
  case fromJSON (toJSON tagged) of
    Error e -> error $ "PgRelation fromJSON: " ++ e
    Success r ->
      unless (r == tagged)
        $ error $ "PgRelation round-trip: got " ++ show r

pgConstraintDuplicateFieldRoundTrip :: IO ()
pgConstraintDuplicateFieldRoundTrip = do
  let
    rel :: PgRelation
    rel =
      PgRelation
        { constraint__namespace = "nspname" =: ("a" :: Text)
        , conname = "b"
        , constraint__class = PgClassShort ("nspname" =: ("c" :: Text)) "d"
        , constraint__fclass = PgClassShort ("nspname" =: ("e" :: Text)) "f"
        , conkey = pgArr' [1 :: Int16, 2]
        , confkey = pgArr' []
        }
    r = "conname" =: ("x" :: Text) :. "conname" =: ("z" :: Text)
      :. rel :. "conname" =: ("y" :: Text)
    tagged = PgTag @AnnCons r
  case fromJSON (toJSON tagged) of
    Error e -> error $ "PgRelation duplicate conname fromJSON: " ++ e
    Success res ->
      unless (res == tagged)
        $ error $ "PgRelation duplicate conname round-trip: got " ++ show res
