{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils where

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
import Data.Text as T
import Data.Time
import Data.UUID.Types
import qualified Data.Vector as Vec
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray(..))
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude as P
import PgSchema.DML
import Sch
import GHC.TypeLits (KnownSymbol)


type TS tab = "test_pgs" ->> tab


type EnumPGS s = PGEnum Sch ("test_pgs" ->> s)

data RenamerSch :: Renamer

type instance Apply RenamerSch s = RenamerSchImpl s

type family RenamerSchImpl s where
  RenamerSchImpl "leaf_mid2_rev_fk" = "leaf_mid2_fk"
  RenamerSchImpl "mid1_root_fk2" = "mid1_root_fk"
  RenamerSchImpl s = CamelToSnake s

withRollback :: Connection -> IO a -> IO a
withRollback conn act = execute_ conn "BEGIN" >> act `finally` rollback conn

withPool :: Pool Connection -> (Connection -> IO a) -> IO a
withPool pool a = Pool.withResource pool \conn -> withRollback conn (a conn)

type AnnSch tn = 'Ann RenamerSch Sch (TS tn)

insSch
  :: forall tn -> forall r r'. InsertReturning (AnnSch tn) r r'
  => Connection -> [r] -> IO ([r'], Text)
insSch tn = insertSch (AnnSch tn)

insSch_
  :: forall tn -> forall r. InsertNonReturning (AnnSch tn) r
  => Connection -> [r] -> IO (Int64, Text)
insSch_ tn = insertSch_ (AnnSch tn)

selSch :: forall tn -> forall r. Selectable (AnnSch tn) r
  => Connection -> QueryParam Sch (TS tn) -> IO ([r], (Text,[SomeToField]))
selSch tn = selectSch (AnnSch tn)

updByCond_
  :: forall tn -> forall r. UpdateNonReturning (AnnSch tn) r
  => Connection -> r -> Cond Sch (TS tn) -> IO Int64
updByCond_ tn = updateByCond_ (AnnSch tn)

updByCond :: forall tn -> forall r r'. UpdateReturning (AnnSch tn) r r'
  => Connection -> r -> Cond Sch (TS tn) -> IO [r']
updByCond tn = updateByCond (AnnSch tn)

delByCond :: forall tn -> ToStar tn
  => Connection -> Cond Sch (TS tn) -> IO (Int64, (Text,[SomeToField]))
delByCond tn = deleteByCond Sch (TS tn)

insJSON_
  :: forall tn -> forall r. (InsertTreeNonReturning RenamerSch Sch (TS tn) r)
  => Connection -> [r] -> IO Text
insJSON_ tn = insertJSON_ RenamerSch Sch (TS tn)

insJSON
  :: forall tn -> forall r r'. (InsertTreeReturning RenamerSch Sch (TS tn) r r')
  => Connection -> [r] -> IO ([r'], Text)
insJSON tn = insertJSON RenamerSch Sch (TS tn)

upsJSON_
  :: forall tn -> forall r. (UpsertTreeNonReturning RenamerSch Sch (TS tn) r)
  => Connection -> [r] -> IO Text
upsJSON_ tn = upsertJSON_ RenamerSch Sch (TS tn)

upsJSON
  :: forall tn -> forall r r'. (UpsertTreeReturning RenamerSch Sch (TS tn) r r')
  => Connection -> [r] -> IO ([r'], Text)
upsJSON tn = upsertJSON RenamerSch Sch (TS tn)

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
  default defGen :: (Generic a, GProdDefault (Rep a)) => Gen a
  defGen = to <$> gprodDefGen

class GProdDefault f where
  gprodDefGen :: Gen (f p)
instance GProdDefault U1 where
  gprodDefGen = pure U1
instance (GProdDefault f, GProdDefault g) => GProdDefault (f :*: g) where
  gprodDefGen = (:*:) <$> gprodDefGen <*> gprodDefGen
instance (GProdDefault f) => GProdDefault (M1 i meta f) where
  gprodDefGen = M1 <$> gprodDefGen
instance (GenDefault c) => GProdDefault (K1 i c) where
  gprodDefGen = K1 <$> defGen

instance GenDefault () where defGen = pure ()
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

genData :: forall a -> GenDefault a => Gen [a]
genData a = genData' a 1 1000

instance GenDefault a => GenDefault [a] where
  defGen = genData' a 0 10

genData' :: forall a -> Int -> Int -> GenDefault a => Gen [a]
genData' a n1 n2 = Gen.list (Range.linear n1 n2) defGen


--genIsoHList
--  :: forall tn r -> forall h.
--    ( IsoHList RenamerSch Sch (TS tn) r
--    , h ~ HList (HListRep RenamerSch Sch (TS tn) r)
--    , GenDefault h )
--  => Gen r
--genIsoHList tn r @h =
--  fromHList @RenamerSch @Sch @(TS tn) @r <$> (defGen :: Gen h)
--
--genDataH
--  :: forall tn r -> forall h
--  . ( IsoHList RenamerSch Sch (TS tn) r
--    , h ~ HList (HListRep RenamerSch Sch (TS tn) r)
--    , GenDefault h )
--  => Int -> Int -> Gen [r]
--genDataH tn r n1 n2 = Gen.list (Range.linear n1 n2) $ genIsoHList tn r
