{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Tree DML ('insertJSON', 'upsertJSON', 'updateJSON') must store 'Nothing'
-- for a nullable @json@ / @jsonb@ column as SQL @NULL@ (not the JSON value
-- @null@), the same as flat 'insertSch'.
module Tests.JsonNull where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Aeson qualified as A
import Data.Functor
import Data.Int
import Data.List qualified as L
import Data.Pool as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..), fromJSONField)
import GHC.Generics (Generic)
import Hedgehog
import PgSchema.DML
import Utils


data Note = Note { note :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | Needed to read a root-level @jsonb@ column into 'Note'.
instance FromField Note where
  fromField = fromJSONField

type RootRec =
  "code" := Text :. "grp" := Int32 :. "name" := Text :. "someEmpty" := ()

-- | @mid2@: primary key @(root_id, seq)@, @payload jsonb@ nullable.
type Mid2In =
  "seq" := Int32 :. "kind" := Text :. "flag" := Bool :. "priority" := Int32
    :. "payload" := Maybe Value

type Mid2Patch = "seq" := Int32 :. "payload" := Maybe Value

mid2 :: Int32 -> Maybe Value -> Mid2In
mid2 s p = "seq" =: s :. "kind" =: "k" :. "flag" =: True :. "priority" =: 1
  :. "payload" =: p

noteVal :: Text -> Value
noteVal t = object ["note" .= t]

-- | @(seq, payload IS NULL)@ for all @mid2@ rows of a root, by @seq@.
mid2Nulls :: Connection -> Int32 -> IO [(Int32, Bool)]
mid2Nulls conn rid = query conn
  "select seq, payload is null from test_pgs.mid2 where root_id = ? order by seq"
  (Only rid)

prop_json_null_tree :: Pool Connection -> Property
prop_json_null_tree pool = withTests 1 $ property do
  ( nullsIns, nullCnt, selNotes, selVals, insSql
    , nullsUps, nullsUpd, nullsUpdNull ) <- evalIO $ withPool pool \conn -> do
    void $ delByCond "mid2" conn mempty
    void $ delByCond "root" conn mempty
    let
      root :: RootRec
      root = "code" =: "jnull" :. "grp" =: 1 :. "name" =: "n" :. "someEmpty" =: ()
    -- insertJSON: Nothing / object / JSON null in child rows
    (ret :: ["id" := Int32], insSql) <- insJSON "root" conn
      [ root :. "mid2_root_fk" =:
        [ mid2 1 Nothing, mid2 2 (Just $ noteVal "a"), mid2 3 (Just A.Null) ] ]
    rid <- case ret of
      [PgTag x] -> pure x
      _ -> fail $ "unexpected insertJSON result: " <> show ret
    nullsIns <- mid2Nulls conn rid
    (nulls :: ["seq" := Int32], _) <- selSch "mid2" conn
      $ qRoot $ qWhere $ pnull "payload"
    -- SQL NULL is read back as Nothing (FromJSON of the record is not called)
    (selNotes :: ["seq" := Int32 :. "payload" := Maybe Note], _) <-
      selSch "mid2" conn $ qRoot $ qOrderBy [ascf "seq"]
    (selVals :: ["seq" := Int32 :. "payload" := Maybe Value], _) <-
      selSch "mid2" conn $ qRoot $ qOrderBy [ascf "seq"]
    -- upsertJSON: ON CONFLICT ... DO UPDATE SET payload = EXCLUDED.payload
    void $ upsJSON_ "root" conn
      [ root :. "mid2_root_fk" =:
        [ mid2 1 (Just $ noteVal "b"), mid2 2 Nothing, mid2 3 (Just A.Null) ] ]
    nullsUps <- mid2Nulls conn rid
    -- updateJSON: UPDATE ... SET payload = ...
    void $ updJSON_ "root" conn
      [ "id" =: rid :. "mid2_root_fk" =:
        [ "seq" =: (1 :: Int32) :. "payload" =: Nothing :: Mid2Patch
        , "seq" =: 2 :. "payload" =: Just (noteVal "c") ] ]
    nullsUpd <- mid2Nulls conn rid
    -- updateJSON: an explicit JSON null value is stored as SQL NULL too
    void $ updJSON_ "root" conn
      [ "id" =: rid :. "mid2_root_fk" =:
        [ "seq" =: (2 :: Int32) :. "payload" =: Just A.Null :: Mid2Patch ] ]
    nullsUpdNull <- mid2Nulls conn rid
    pure ( nullsIns, L.length nulls, selNotes, selVals, insSql
      , nullsUps, nullsUpd, nullsUpdNull )
  annotate $ show insSql
  nullsIns === [(1, True), (2, False), (3, True)]
  nullCnt === 2
  selNotes ===
    [ "seq" =: 1 :. "payload" =: Nothing
    , "seq" =: 2 :. "payload" =: Just (Note "a")
    , "seq" =: 3 :. "payload" =: Nothing ]
  selVals ===
    [ "seq" =: 1 :. "payload" =: Nothing
    , "seq" =: 2 :. "payload" =: Just (noteVal "a")
    , "seq" =: 3 :. "payload" =: Nothing ]
  nullsUps === [(1, False), (2, True), (3, True)]
  nullsUpd === [(1, True), (2, False), (3, True)]
  nullsUpdNull === [(1, True), (2, True), (3, True)]

type JsonCols = "cjsonb" := Maybe Value :. "cjson" := Maybe Value

jsonCols :: Maybe Value -> JsonCols
jsonCols v = "cjsonb" =: v :. "cjson" =: v

-- | @(cjsonb IS NULL, cjson IS NULL)@ for all @ext_converts@ rows.
extNulls :: Connection -> IO [(Bool, Bool)]
extNulls conn = L.sort <$> query_ conn
  "select cjsonb is null, cjson is null from test_pgs.ext_converts"

-- | Both @json@ and @jsonb@ columns; flat 'insertSch' for comparison.
prop_json_null_json_and_jsonb :: Pool Connection -> Property
prop_json_null_json_and_jsonb pool = withTests 1 $ property do
  (viaJSON, viaJSONSel, viaFlat) <- evalIO $ withPool pool \conn -> do
    void $ delByCond "ext_converts" conn mempty
    void $ insJSON_ "ext_converts" conn
      [jsonCols Nothing, jsonCols (Just $ noteVal "a"), jsonCols (Just A.Null)]
    viaJSON <- extNulls conn
    (sel :: [JsonCols], _) <- selSch "ext_converts" conn qpEmpty
    void $ delByCond "ext_converts" conn mempty
    -- flat insert keeps an explicit JSON null value as the JSON value null
    void $ insSch_ "ext_converts" conn [jsonCols Nothing, jsonCols (Just A.Null)]
    viaFlat <- extNulls conn
    pure (viaJSON, sel, viaFlat)
  viaJSON === [(False, False), (True, True), (True, True)]
  L.sort viaJSONSel === L.sort
    [jsonCols Nothing, jsonCols (Just $ noteVal "a"), jsonCols Nothing]
  viaFlat === [(False, False), (True, True)]
