{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Exception
import Data.Aeson as A (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive
import Data.Functor
import qualified Data.HashMap.Strict as Map
import Data.Int
import Data.List qualified as L
import Data.Pool as Pool
import Data.Scientific
import Data.String as S
import Data.Text as T
import Data.Time
import Data.UUID.Types
import qualified Data.Vector as Vec
import Database.PostgreSQL.Simple
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude as P
import System.Environment
import Test.Tasty
import Test.Tasty.Hedgehog


import PgSchema
import PgSchema.Util
import Sch

main :: IO ()
main = do
  connStr <- maybe "dbname=schema_test" BS.pack <$> lookupEnv "PG_CONN"
  pool <- newPool $ defaultPoolConfig (connectPostgreSQL connStr) close 10 10
  defaultMain $ testGroup "DB Tests"
    [ testProperty "We can insert base types, select, update and get their returnings" $ prop_base_converts pool
    , testProperty "We can insert array of base types (excluding dates), select and get their returnings" $ prop_base_arr_converts pool
    , testProperty "We can insert extra types (bytea, jsonb, enums, uuid), select and get their returnings" $ prop_ext_converts pool
    , testProperty "We can't insert array of extra types. Only citext[] can be inserted :-(" $ prop_ext_arr_converts pool
    ]

type BaseConverts = "cboolean" := Maybe Bool
  :.. "cint4" := Maybe Int32
  :.. "cfloat8" := Maybe Double
  :.. "cdate" := Maybe Day
  :.. "ctime" := Maybe TimeOfDay
  :.. "ctimestamp" := Maybe LocalTime
  :.. "ctimestamptz" := Maybe UTCTime
  :.. "ctext" := Maybe Text

-- We can't insert arrays of dates (workaround: insertJSON)
type BaseArrConverts = "cboolean" := Maybe (PgArr Bool)
  :.. "cint4" := Maybe (PgArr Int32)
  :.. "cfloat8" := Maybe (PgArr Double)
  :.. "ctext" := Maybe (PgArr Text)

type ExtConverts = "ccitext" := Maybe (CI Text)
  :.. "ccolor" := Maybe (PGEnum Sch ( "test_schema" ->> "color" ))
  :.. "cbytea" := Maybe (Binary BS.ByteString)
  :.. "cjsonb" := Maybe Value
  :.. "cjson" := Maybe Value
  :.. "cuuid" := Maybe UUID

type ExtArrConverts = "ccitext" := Maybe (PgArr (CI Text))
  -- :.. "ccolor" := Maybe (PgArr (PGEnum Sch ( "test_schema" ->> "color" )))
  -- :.. "cbytea" := Maybe (PgArr (Binary BS.ByteString))
  -- :.. "cjsonb" := Maybe (PgArr Value)
  -- :.. "cuuid" := Maybe (PgArr UUID)

genDay :: Gen Day
-- genDay = ModifiedJulianDay . fromIntegral <$> Gen.int (Range.linear (-100000) 200000)
-- genDay = ModifiedJulianDay . fromIntegral <$> Gen.int (Range.linear 10000 100000)
genDay = ModifiedJulianDay . fromIntegral <$> Gen.int (Range.linear 50000 80000)

genTime :: Gen TimeOfDay
genTime = timeToTimeOfDay . fromIntegral <$> Gen.int (Range.linear 0 86399)

genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTime

genUTCTime :: Gen UTCTime
genUTCTime = do
  day <- genDay
  diff <- fromIntegral <$> Gen.int (Range.linear 0 86399)
  pure $ UTCTime day diff

class GenDefault a where
  defGen :: Gen a

instance GenDefault Int32 where defGen = Gen.int32 (Range.linear 0 1000000)
instance GenDefault Text where defGen = Gen.text (Range.linear 0 50) Gen.alpha
instance GenDefault Bool where defGen = Gen.bool
instance GenDefault Double where defGen = Gen.double $ Range.linearFrac 0 1000000
instance GenDefault Day where defGen = genDay
instance GenDefault TimeOfDay where defGen = genTime
instance GenDefault LocalTime where defGen = genLocalTime
instance GenDefault UTCTime where defGen = genUTCTime
instance GenDefault a => GenDefault (Maybe a) where defGen = Gen.maybe defGen

instance GenDefault a => GenDefault (PgArr a) where
  defGen = PgArr <$> Gen.list (Range.linear 0 5) defGen

instance GenDefault val => GenDefault (tag := val) where
  defGen = (tag =:) <$> defGen

instance (GenDefault h, GenDefault t) => GenDefault (h :.. t) where
  defGen = (:..) <$> defGen <*> defGen

instance GenDefault (PGEnum Sch (TS "color")) where defGen = Gen.enumBounded

instance GenDefault Value where defGen = genValue

instance GenDefault (CI Text) where defGen = S.fromString . T.unpack <$> defGen

instance GenDefault (Binary BS.ByteString) where defGen = Binary <$> Gen.bytes (Range.linear 0 512)

instance GenDefault UUID where defGen = genUUID

genBaseConvertsList :: Gen [BaseConverts]
genBaseConvertsList = Gen.list (Range.linear 1 10000) defGen

genBaseArrConvertsList :: Gen [BaseArrConverts]
genBaseArrConvertsList = Gen.list (Range.linear 1 10000) defGen

genExtConvertsList :: Gen [ExtConverts]
genExtConvertsList = Gen.list (Range.linear 1 10000) defGen

genExtArrConvertsList :: Gen [ExtArrConverts]
genExtArrConvertsList = Gen.list (Range.linear 1 10000) defGen

genScientific :: Gen Scientific
genScientific =
  scientific
    <$> Gen.integral (Range.linear (-10000) 10000) -- коэффициент
    <*> Gen.int (Range.linear (-10) 10)           -- экспонента

genUUID :: Gen UUID
genUUID = do
  -- UUID состоит из 16 байт
  bytes <- Gen.bytes (Range.singleton 16)
  case fromByteString (BSL.fromStrict bytes) of
    Nothing -> error "Should never happen: 16 bytes is correct for UUID"
    Just u  -> pure u

genValue :: Gen Value
genValue = Gen.recursive Gen.choice
  -- Базовые типы (листья дерева)
  [ pure A.Null
  , Bool   <$> Gen.bool
  , Number <$> genScientific
  , String <$> Gen.text (Range.linear 0 20) Gen.unicode
  ]
  -- Рекурсивные типы (узлы дерева)
  [ -- Генерация массива
    Array . Vec.fromList <$> Gen.list (Range.linear 0 5) genValue
    -- Генерация объекта (ключ-значение)
  , Object . KeyMap.fromList <$> Gen.list (Range.linear 0 5) genPair
  ]
  where
    genPair = (,) . Key.fromText
      <$> Gen.text (Range.linear 1 15) Gen.alpha
      <*> genValue

type TS tab = "test_schema" ->> tab

prop_base_converts :: Pool Connection -> Property
prop_base_converts pool = withTests 30 $ property do
  recs <- forAll genBaseConvertsList
  (resSel, resIns, resUpd) <- evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insertSch_ Sch (TS "base_converts") RenamerId conn recs
      (res, _) <- selectSch Sch (TS "base_converts") RenamerId conn qpEmpty
      res'' <- updateByCond @Sch @(TS "base_converts") conn
        ("cint4" =: Just (10 :: Int32)) ("cboolean" =? Just False)
      (res', _) <- insertSch Sch (TS "base_converts") RenamerId conn recs
      pure (res, res', res'')
  L.sort resSel === L.sort recs
  resIns === recs -- add sort when fail
  L.sort resUpd === L.sort ((\(a:..b:..c) -> a:.. "cint4" =: Just (10::Int32) :..c)
    <$> L.filter (\(PgTag a:.._) -> a == Just False) recs)

prop_base_arr_converts :: Pool Connection -> Property
prop_base_arr_converts pool = withTests 30 $ property do
  (recs, ds) <- forAll
    $ (,) <$> genBaseArrConvertsList <*> Gen.list (Range.linear 0 20) genUTCTime
  (resSel, resIns, resUpd) <- evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insertSch_ Sch (TS "base_arr_converts") RenamerId conn recs
      (res, _) <- selectSch Sch (TS "base_arr_converts") RenamerId conn qpEmpty
      (resu :: ["ctimestamptz" := Maybe (PgArr UTCTime) :.. BaseArrConverts])
        <- updateByCond @Sch @(TS "base_arr_converts") conn
          ("ctimestamptz" =: Just (PgArr ds)) mempty
      (res', _) <- insertSch Sch (TS "base_arr_converts") RenamerId conn recs
      pure (res, res',resu)
  L.length resSel === L.length resUpd
  L.sort resSel === L.sort recs
  fmap (\(PgTag @"ctimestamptz" (Nothing @(PgArr UTCTime)) :.. r) -> r) resIns === recs -- add sort when fail

prop_ext_converts :: Pool Connection -> Property
prop_ext_converts pool = withTests 30 $ property do
  recs <- forAll genExtConvertsList
  (resSel, resIns, resUpd) <- evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insertSch_ Sch (TS "ext_converts") RenamerId conn recs
      (res, _) <- selectSch Sch (TS "ext_converts") RenamerId conn qpEmpty
      res'' <- updateByCond @Sch @(TS "ext_converts") conn
        ("ccitext" =: Just ("CaSes" :: CI Text)) ("ccolor" =? Just Color_red)
      (res', _) <- insertSch Sch (TS "ext_converts") RenamerId conn recs
      pure (res, res', res'')
  L.sort resSel === L.sort recs
  resIns === recs -- add sort when fail
  L.sort resUpd === L.sort ((\(a:..b) -> "ccitext" =: Just ("cAses" :: CI Text) :..b)
    <$> L.filter (\(_:..PgTag a:.._) -> a == Just Color_red) recs)

prop_ext_arr_converts :: Pool Connection -> Property
prop_ext_arr_converts pool = withTests 30 $ property do
  recs <- forAll genExtArrConvertsList
  evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insertSch_ Sch (TS "ext_arr_converts") RenamerId conn recs

withRollback :: Connection -> IO a -> IO a
withRollback conn act = execute_ conn "BEGIN" >> act `finally` rollback conn
