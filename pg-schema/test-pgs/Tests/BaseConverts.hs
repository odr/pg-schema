{-# LANGUAGE BlockArguments #-}
module Tests.BaseConverts where

import Control.Exception
import Data.Aeson as A (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive
import Data.Functor
import Data.Int
import Data.List qualified as L
import Data.Pool as Pool
import Data.Scientific
import Data.String as S
import Data.Tagged
import Data.Text as T
import Data.Time
import Data.UUID.Types
import qualified Data.Vector as Vec
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude as P
import PgSchema
import PgSchema.Util
import Sch

type TS tab = "test_schema" ->> tab

type BaseConverts = "cboolean" := Maybe Bool
  :. "cint4" := Maybe Int32
  :. "cdate" := Maybe Day
  :. "ctime" := Maybe TimeOfDay
  :. "ctimestamp" := Maybe LocalTime
  :. "ctimestamptz" := Maybe UTCTime
  :. "ctext" := Maybe Text
  :. "cfloat8" := Maybe Double

type BaseArrConverts = "cboolean" := Maybe (PgArr Bool)
  :. "cint4" := Maybe (PgArr Int32)
  :. "cfloat8" := Maybe (PgArr Double)
  :. "ctext" := Maybe (PgArr Text)
  :. "ctimestamptz" := Maybe (PgArr UTCTime)

type ExtConverts = "ccitext" := Maybe (CI Text)
  :. "ccolor" := Maybe (PGEnum Sch ( "test_schema" ->> "color" ))
  :. "cbytea" := Maybe (Binary BS.ByteString)
  :. "cjsonb" := Maybe Value
  :. "cjson" := Maybe Value
  :. "cuuid" := Maybe UUID

type ExtArrConverts = "ccitext" := Maybe (PgArr (CI Text))
  :. "ccolor" := Maybe (PgArr (PGEnum Sch ( "test_schema" ->> "color" )))
  :. "cbytea" := Maybe (PgArr (Binary BS.ByteString))
  :. "cjsonb" := Maybe (PgArr Value)
  :. "cuuid" := Maybe (PgArr UUID)

genDay :: Gen Day
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

instance (GenDefault h, GenDefault t) => GenDefault (h :. t) where
  defGen = (:.) <$> defGen <*> defGen

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
    <$> Gen.integral (Range.linear (-10000) 10000)
    <*> Gen.int (Range.linear (-10) 10)

genUUID :: Gen UUID
genUUID = do
  bytes <- Gen.bytes (Range.singleton 16)
  case fromByteString (BSL.fromStrict bytes) of
    Nothing -> error "Should never happen: 16 bytes is correct for UUID"
    Just u  -> pure u

genValue :: Gen Value
genValue = Gen.recursive Gen.choice
  [ pure A.Null
  , Bool   <$> Gen.bool
  , Number <$> genScientific
  , String <$> Gen.text (Range.linear 0 20) Gen.unicode
  ]
  [ Array . Vec.fromList <$> Gen.list (Range.linear 0 5) genValue
  , Object . KeyMap.fromList <$> Gen.list (Range.linear 0 5) genPair
  ]
  where
    genPair = (,) . Key.fromText
      <$> Gen.text (Range.linear 1 15) Gen.alpha
      <*> genValue

insSch
  :: forall tn -> forall r r' h h'
  . InsertReturning' RenamerId Sch (TS tn) r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
insSch tn = insertSch RenamerId Sch (TS tn)

insSch_
  :: forall tn -> forall r h
  . InsertNonReturning' RenamerId Sch (TS tn) r h
  => Connection -> [r] -> IO (Int64, Text)
insSch_ tn = insertSch_ RenamerId Sch (TS tn)

type HSch s r = HListTag (HListTagRep RenamerId Sch (TS s) r)

selSch :: forall tn -> forall r h.
  ( IsoHListTag RenamerId Sch (TS tn) r, h ~ HSch tn r
  , CHListInfo Sch (TS tn) h, FromRow h )
  => Connection -> QueryParam Sch (TS tn) -> IO ([r], (Text,[SomeToField]))
selSch tn = selectSch RenamerId Sch (TS tn)

updByCond_
  :: forall tn -> forall r h.
  ( h ~ HRep RenamerId Sch (TS tn) r, HListInfo RenamerId Sch (TS tn) r h
  , ToRow h, AllPlain Sch (TS tn) h )
  => Connection -> r -> Cond Sch (TS tn) -> IO Int64
updByCond_ tn = updateByCond_ RenamerId Sch (TS tn)

updByCond :: forall tn -> forall r r' h h'.
  ( UpdateReturning RenamerId Sch (TS tn) r r' h h'
  , AllPlain Sch (TS tn) h, ToRow h, FromRow h' )
  => Connection -> r -> Cond Sch (TS tn) -> IO [r']
updByCond tn = updateByCond RenamerId Sch (TS tn)

withRollback :: Connection -> IO a -> IO a
withRollback conn act = execute_ conn "BEGIN" >> act `finally` rollback conn

prop_base_converts :: Pool Connection -> Property
prop_base_converts pool = withTests 30 $ property do
  recs <- forAll genBaseConvertsList
  (resSel, resIns, resUpd) <- evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insSch_ "base_converts" conn recs
      (res, _) <- selSch "base_converts" conn qpEmpty
      res'' <- updByCond "base_converts" conn
        ("cint4" =: Just (10 :: Int32)) ("cboolean" =? Just False)
      (res', _) <- insSch "base_converts" conn recs
      pure (res, res', res'')
  L.sort resSel === L.sort recs
  resIns === recs
  L.sort resUpd === L.sort ((\(a:.b:.c) -> a:. "cint4" =: Just (10::Int32) :.c)
    <$> L.filter (\(Tagged a:._) -> a == Just False) recs)

prop_base_arr_converts :: Pool Connection -> Property
prop_base_arr_converts pool = withTests 30 $ property do
  recs <- forAll genBaseArrConvertsList
  ds <- forAll $ Gen.list (Range.linear 0 20) genUTCTime
  (resSel, resIns, resUpd::[BaseArrConverts]) <- evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insSch_ "base_arr_converts" conn recs
      (res, _) <- selSch "base_arr_converts" conn qpEmpty
      res'' <- updByCond "base_arr_converts" conn
        ("ctimestamptz" =: Just (pgArr' ds)) mempty
      (res', _) <- insSch "base_arr_converts" conn recs
      pure (res, res', res'')
  L.sort resSel === L.sort recs
  resIns === recs
  L.length resUpd === L.length recs

prop_ext_converts :: Pool Connection -> Property
prop_ext_converts pool = withTests 30 $ property do
  recs <- forAll genExtConvertsList
  (resSel, resIns, resUpd) <- evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insSch_ "ext_converts" conn recs
      (res, _) <- selSch "ext_converts" conn qpEmpty
      res'' <- updByCond "ext_converts" conn
        ("ccitext" =: Just ("CaSes" :: CI Text)) ("ccolor" =? Just Color_red)
      (res', _) <- insSch "ext_converts" conn recs
      pure (res, res', res'')
  L.sort resSel === L.sort recs
  resIns === recs
  L.sort resUpd === L.sort ((\(a:.b) -> "ccitext" =: Just ("cAses" :: CI Text) :.b)
    <$> L.filter (\(_ :. Tagged a :. _) -> a == Just Color_red) recs)

prop_ext_arr_converts :: Pool Connection -> Property
prop_ext_arr_converts pool = withTests 30 $ property do
  recs <- forAll genExtArrConvertsList
  evalIO $ Pool.withResource pool \conn ->
    withRollback conn do
      void $ insSch_ "ext_arr_converts" conn recs
