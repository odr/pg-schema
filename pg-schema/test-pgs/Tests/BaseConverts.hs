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
import Sch
import Utils


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
  :. "ccolor" := Maybe (EnumPGS "color" )
  :. "cbytea" := Maybe (Binary BS.ByteString)
  :. "cjsonb" := Maybe Value
  :. "cjson" := Maybe Value
  :. "cuuid" := Maybe UUID

type ExtArrConverts = "ccitext" := Maybe (PgArr (CI Text))
  :. "ccolor" := Maybe (PgArr (EnumPGS "color" ))
  :. "cbytea" := Maybe (PgArr (Binary BS.ByteString))
  :. "cjsonb" := Maybe (PgArr Value)
  :. "cuuid" := Maybe (PgArr UUID)

prop_base_converts :: Pool Connection -> Property
prop_base_converts pool = withTests 30 $ property do
  recs <- forAll (genData BaseConverts)
  (resSel, resIns, resUpd) <- evalIO $ withPool pool \conn -> do
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
  recs <- forAll (genData BaseArrConverts)
  ds <- forAll $ Gen.list (Range.linear 0 20) genUTCTime
  (resSel, resIns, resUpd::[BaseArrConverts]) <- evalIO $ withPool pool \conn -> do
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
  recs <- forAll (genData ExtConverts)
  (resSel, resIns, resUpd) <- evalIO $ withPool pool \conn -> do
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
  recs <- forAll (genData ExtArrConverts)
  evalIO $ withPool pool \conn -> do
    void $ insSch_ "ext_arr_converts" conn recs
