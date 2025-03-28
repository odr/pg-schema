{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Fixed
import Data.List as L
import Data.Text as T
import Data.Text.IO as T
import Data.Time
import Data.Singletons
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Generic.Random
import GHC.Generics
import GHC.Int
import PgSchema
import Database.PostgreSQL.DML.InsertJSON qualified as I2
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
  , city_country :: Maybe Country }
  deriving (Eq, Show, Ord, Generic)

data Address = Address
  { street       :: Maybe Text
  , home         :: Maybe Text
  , app          :: Maybe Text
  , zipcode      :: Maybe Text
  , address_city :: Maybe City } -- PgTagged "name" (Maybe Text) }
  deriving (Eq, Show, Ord, Generic)

data Company = Company
  { name       :: Text
  , address_id :: Maybe Int32 }
  deriving (Eq, Show, Generic)

data Article = Article
  { name :: Text
  , code :: Maybe Text }
  deriving (Eq, Show, Generic)

data OrdPos = OrdPos
  { num          :: Int32
  , opos_article :: Article
  , cnt          :: Int32
  , price        :: Centi }
  deriving (Eq, Show, Generic)

-- data Customer = Customer
--   { }
data Order = Order
  { day        :: Day
  , num        :: Text
  , ord_seller :: Company
  , opos_order :: SchList OrdPos
  , state      :: Maybe (PGEnum Sch ("sch" ->> "order_state")) }
  deriving (Eq, Show, Generic)

data OrdPosI = OrdPosI
  { num          :: Int32
  , article_id   :: Int32
  , cnt          :: Int32
  , price        :: Centi }
  deriving (Eq, Show, Generic)
  deriving anyclass ToJSON

data OrderI = OrderI
  { day        :: Day
  , num        :: Text
  , seller_id  :: Int32
  , state      :: Maybe (PGEnum Sch ("sch" ->> "order_state"))
  , opos_order :: SchList OrdPosI }
  deriving (Eq, Show, Generic, ToJSON)

data CustomerI = CustomerI
  { name :: Text
  -- , ord_cust :: SchList (PgTagged
  --   '["num", "opos_order", "day", "seller_id"]
  --   (Text, (SchList OrdPosI, (Day, Int32))))
  , ord_cust :: SchList OrderI
  }
  deriving (Eq, Show, Generic, ToJSON)

data CompanyI = CompanyI
  { name :: Text }
  deriving (Eq, Show, Generic, ToJSON)

data AddressI = AddressI
  { street :: Maybe Text
  , zipcode :: Maybe Text
  , cust_addr :: SchList CustomerI
  , comp_addr :: SchList CompanyI }
  deriving (Eq, Show, Generic, ToJSON)


schemaRec id ''OrderI
schemaRec id ''OrdPosI
schemaRec id ''CustomerI
schemaRec id ''CompanyI
schemaRec id ''AddressI
instance CDmlRecord PG Sch ("sch" ->> "addresses") AddressI

deriveDmlRecord id ''Sch [ (''Country, "sch" ->> "countries") ]

deriveQueryRecord id ''Sch
  [ (''City, "sch" ->> "cities")
  , (''Address, "sch" ->> "addresses")
  , (''Company, "sch" ->> "companies")
  , (''Article, "sch" ->> "articles") ]

  -- No instance for (ToField (Data.Fixed.Fixed E2))
  -- so these not compiled:
  --
  -- , (''OrdPos, [t|OrdPos|], "sch" ->> "order_positions")
  -- , (''Order, [t|Order|], "sch" ->> "orders") ]

L.concat
  <$> traverse (\(n,s) ->
    L.concat <$> sequenceA
      [ deriveJSON defaultOptions n
      -- , [d|instance FromRow $(conT n)|]
      -- No instance for (ToField (Data.Fixed.Fixed E2))
      -- , [d|instance ToRow $(conT n)|]
      -- No instance for (ToField (Data.Fixed.Fixed E2))
      , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
      , [d|instance ToField $(liftType n) where toField = toJSONField |]
      , schemaRec id n
      , [d|instance CQueryRecord PG Sch $(liftType s) $(liftType n)|]
      ])
  [ (''OrdPos, "sch" ->> "order_positions")
  , (''Order, "sch" ->> "orders") ]

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
  mapM_ (print @(PgTagged "id" Int32)) cids
  d <- utctDay <$> getCurrentTime
  -- T.putStrLn $ I2.insertJSONText @Sch @(NSC "addresses") @AddressI @(PgTagged "id" Int32)
  let
    insData =
      [ AddressI (Just "street") Nothing (SchList
        [ CustomerI "Ivan" $ SchList
          [ OrderI d "1" 1 (Just Order_state_paid) $ SchList
            [ OrdPosI 1 2 3 4, OrdPosI 2 3 4 5 ]
          , OrderI d "2a" 3 (Just Order_state_booked) $ SchList
            [ OrdPosI 3 2 3 4, OrdPosI 1 3 4 5.1 ] ]
        , CustomerI "Petr" $ SchList
          [ OrderI d "1v" 4 (Just Order_state_paid) $ SchList
            [ OrdPosI 1 2 3 4, OrdPosI 2 3 4 5 ]
          , OrderI d "xx" 5 (Just Order_state_delivered) $ SchList
            [ OrdPosI 5 6 3 4, OrdPosI 1 3 3 5.1 ] ] ]) mempty
      , AddressI Nothing (Just "zipcode") mempty $ SchList [CompanyI "Typeable"]
      , AddressI (Just "street2") (Just "zip2") (SchList [CustomerI "Dima" mempty])
        $ SchList [CompanyI "WellTyped"] ]
  as1 :: [PgTagged '["id", "cust_addr"] (Int32, SchList (PgTagged "id" Int32))]
    <- I2.insertJSON @Sch @(NSC "addresses") @AddressI conn insData
  -- as1 :: [PgTagged "id" Int32] <- I2.insertJSON @Sch @(NSC "addresses") @AddressI conn insData
  Prelude.putStrLn $ show as1
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
            $ pparent @(NSC "city_country") (pcmp @"code" =? Just @Text "RU"))]
      , qpOrds = [ rootOrd [ascf @"street"] ] }
    qp' = qp { qpLOs = [rootLO $ LO (Just 1) (Just 1)] }
