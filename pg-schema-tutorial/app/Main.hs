{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Fixed
import Data.List as L
import Data.Text as T
import Data.Text.IO as T
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Generic.Random
import GHC.Generics
import PgSchema
import Sch
import Test.QuickCheck
import Test.QuickCheck.Instances ()

data Country = Country
  { code :: Maybe Text
  , name :: Text }
  -- TODO: cycle references lead to halt! Should check to avoid it
  -- , city_country :: SchList City }
  deriving (Eq, Show, Ord, Generic)

instance Arbitrary Country where
  arbitrary = genericArbitrarySingle

data City = City
  { name         :: Maybe Text
  , city_country :: Country }
  deriving (Eq, Show, Ord, Generic)

data Address = Address
  { street       :: Maybe Text
  , home         :: Maybe Text
  , app          :: Maybe Text
  , zipcode      :: Maybe Text
  , address_city :: City } -- PgTagged "name" (Maybe Text) }
  deriving (Eq, Show, Ord, Generic)

data Company = Company
  { name       :: Text
  , address_id :: Maybe Int }
  deriving (Eq, Show, Generic)

data Article = Article
  { name :: Text
  , code :: Maybe Text }
  deriving (Eq, Show, Generic)

data OrdPos = OrdPos
  { num          :: Int
  , opos_article :: Article
  , cnt          :: Int
  , price        :: Centi }
  deriving (Eq, Show, Generic)

data Order = Order
  { day        :: Day
  , num        :: Text
  , ord_seller :: Company
  , opos_order :: SchList OrdPos
  , state      :: Maybe (PGEnum Sch ('NameNS "sch" "order_state")) }
  deriving (Eq, Show, Generic)

L.concat
  <$> zipWithM (\n s ->
    L.concat <$> sequenceA
      [ deriveJSON defaultOptions n
      , [d|instance FromRow $(liftType n)|]
      , [d|instance ToRow $(liftType n)|]
      , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
      , [d|instance ToField $(liftType n) where toField = toJSONField |]
      , schemaRec @Sch id n
      , [d|instance CQueryRecord PG Sch $(liftType s) $(liftType n)|]
      ])
  [ ''Country, ''City, ''Address, ''Company, ''Article ]
    -- , ''OrdPos, ''Order]
  [ "sch" ->> "countries", "sch" ->> "cities", "sch" ->> "addresses"
  , "sch" ->> "companies", "sch" ->> "articles" ]
    -- , "order_positions", "orders"]
L.concat
  <$> zipWithM (\n s ->
    L.concat <$> sequenceA
      [ deriveJSON defaultOptions n
      -- , [d|instance FromRow $(conT n)|]
      -- , [d|instance ToRow $(conT n)|]
      , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
      , [d|instance ToField $(liftType n) where toField = toJSONField |]
      , schemaRec @Sch id n
      , [d|instance CQueryRecord PG Sch $(liftType s) $(liftType n)|]
      ])
  [ ''OrdPos, ''Order]
  [ "sch" ->> "order_positions", "sch" ->> "orders"]

type NSC name = 'NameNS "sch" name
main :: IO ()
main = do
  countries <- generate $ replicateM 5 (arbitrary @Country)
  mapM_ (\(a,b) -> T.putStrLn a >> print b)
    [ selectText @Sch @(NSC "countries") @Country qpEmpty
    , selectText @Sch @(NSC "cities") @City qpEmpty
    , selectText @Sch @(NSC "addresses") @Address qpEmpty
    , selectText @Sch @(NSC "addresses") @Address qp
    , selectText @Sch @(NSC "addresses") @Address qp'
    ]
  conn <- connectPostgreSQL "dbname=schema_test user=avia host=localhost"
  cids <- insertSch @Sch @(NSC "countries") conn countries
  mapM_ (print @(PgTagged "id" Int)) cids
  selectSch @Sch @(NSC "countries") @Country conn qpEmpty >>= print
  T.putStrLn ""
  selectSch @Sch @(NSC "cities") @City conn qpEmpty >>= print
  T.putStrLn ""
  selectSch @Sch @(NSC "addresses") @Address conn qpEmpty >>= print
  T.putStrLn ""
  selectSch @Sch @(NSC "addresses") @Address conn qp >>= print
  T.putStrLn ""
  selectSch @Sch @(NSC "addresses") @Address conn qp' >>= print
  where
    qp = qpEmpty
      { qpConds =
        [rootCond
          (pparent @(NSC "address_city")
            $ pparent @(NSC "city_country") (#code =? Just @Text "RU"))]
      , qpOrds = [ rootOrd [ascf @"street"] ] }
    qp' = qp { qpLOs = [rootLO $ LO (Just 1) (Just 1)] }
