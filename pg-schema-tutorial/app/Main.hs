{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Bitraversable
import Data.Fixed
import Data.List as L
import Data.Singletons
import Data.Text as T
import Data.Text.IO as T
import Data.Time
import Data.Traversable
import Data.Type.Bool
import Data.Type.Equality
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Generic.Random
import GHC.Generics
import GHC.Int
import PgSchema
import Database.PostgreSQL.DML.InsertJSON qualified as I2
import Database.PostgreSQL.DML.Update
import Sch
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Instances ()
import Database.Types.EmptyField (EmptyField)
import Prelude as P

data Country = MkCountry
  { code :: Maybe Text
  , name :: Text }
  -- TODO: cycle references lead to halt! Should check to avoid it
  -- , city_country :: SchList City }
  deriving (Eq, Ord, Generic)

instance Arbitrary Country where
  arbitrary = genericArbitrarySingle

deriveQueryRecord id ''Sch (tabInfoMap @Sch)
  [ ((''Country, []), "sch" ->> "countries") ]

data A = A1 | A2
data B = B1 | B2

data Address (a::A) (b::B) = MkAddress
  { street  :: Maybe Text
  , home    :: If (a == A1) (Maybe Text) EmptyField
  , app     :: Maybe Text
  , zipcode :: Maybe Text
  , phones  :: Maybe (PgArr Text)
  , numbers :: Maybe (PgArr Int32) }
  deriving Generic

data City a b = MkCity
  { name         :: Maybe Text
  , city_country :: If (a == A1) EmptyField (Maybe Country)
  , address_city :: SchList (Address a b) }
  deriving Generic

data AddressRev a b = MkAddressRev
  { street       :: Maybe Text
  , home         :: Maybe Text
  , app          :: If (a == A1) (Maybe Text) EmptyField
  , zipcode      :: Maybe Text
  , address_city :: Maybe (City a b) }
  deriving Generic

data Company = MkCompany
  { name       :: Text
  , address_id :: Maybe Int32 }
  deriving Generic

data Article = MkArticle
  { name :: Text
  , code :: Maybe Text }
  deriving Generic

data OrdPos = MkOrdPos
  { num          :: Int32
  , opos_article :: Article
  , cnt          :: Int32
  , price        :: Centi }
  deriving Generic

-- data Customer = Customer
--   { }
data Order = MkOrder
  { day        :: Day
  , num        :: Text
  , ord_seller :: Company
  , opos_order :: SchList OrdPos
  , state      :: Maybe (PGEnum Sch ("sch" ->> "order_state")) }
  deriving Generic

data OrdPosI = MkOrdPosI
  { num          :: Int32
  , article_id   :: Int32
  , cnt          :: Int32
  , price        :: Centi }
  deriving Generic

data OrderI = MkOrderI
  { day        :: Day
  , num        :: Text
  , seller_id  :: Int32
  , state      :: Maybe (PGEnum Sch ("sch" ->> "order_state"))
  , opos_order :: SchList OrdPosI }
  deriving Generic

data CustomerI = MkCustomerI
  { name :: Text
  -- , ord_cust :: SchList (PgTagged
  --   '["num", "opos_order", "day", "seller_id"]
  --   (Text, (SchList OrdPosI, (Day, Int32))))
  , ord_cust :: SchList OrderI
  }
  deriving Generic

data CompanyI = MkCompanyI
  { name :: Text }
  deriving Generic

data AddressI = MkAddressI
  { street :: Maybe Text
  , zipcode :: Maybe Text
  , phones :: Maybe (PgArr Text)
  , numbers :: Maybe (PgArr Int32)
  , cust_addr :: SchList CustomerI
  , comp_addr :: SchList CompanyI }
  deriving Generic

instance HasResolution a => FromField (Fixed a) where
  fromField f mb = (realToFrac :: Rational -> Fixed a) <$> fromField f mb

instance HasResolution a => ToField (Fixed a) where
  toField = toField . (realToFrac :: Fixed a -> Double)

newtype CustId = CustId { id :: Int32 }
  deriving Generic

data AddressRet = AddressRet
  { id :: Int32
  , cust_addr :: SchList (PgTagged "id" Int32)}
  deriving Generic

deriveQueryRecord P.id ''Sch (tabInfoMap @Sch)
  [ ((''Company,[]), "sch" ->> "companies")
  , ((''Article,[]), "sch" ->> "articles")
  , ((''Address, [['A1,'B1],['A2,'B1]]), "sch" ->> "addresses")
  , ((''City, [['A1,'B1],['A2,'B1]]), "sch" ->> "cities")
  , ((''AddressRev, [['A1,'B1],['A2,'B1]]), "sch" ->> "addresses")
  , ((''OrdPosI, []), ("sch" ->> "order_positions"))
  , ((''OrderI, []), ("sch" ->> "orders"))
  , ((''CustomerI, []), ("sch" ->> "customers"))
  , ((''CompanyI, []), ("sch" ->> "companies"))
  , ((''AddressI, []), ("sch" ->> "addresses"))
  , ((''OrdPos, []), "sch" ->> "order_positions")
  , ((''Order, []), "sch" ->> "orders")
  , ((''CustId, []), "sch" ->> "customers")
  , ((''AddressRet,[]), "sch" ->> "addresses")
  ]

type NSC name = "sch" ->> name
main :: IO ()
main = do
  countries <- generate $ replicateM 5 (arbitrary @Country)
  mapM_ (\(a,b) -> T.putStrLn a >> print b)
    [ selectText @Sch @(NSC "countries") @Country qpEmpty
    , selectText @Sch @(NSC "cities") @(City A1 B1) qpEmpty
    , selectText @Sch @(NSC "cities") @(City A2 B1) qpEmpty
    , selectText @Sch @(NSC "addresses") @(Address A1 B1) qpEmpty
    , selectText @Sch @(NSC "addresses") @(Address A2 B1) qp
    , selectText @Sch @(NSC "addresses") @(AddressRev A1 B1) qp
    , selectText @Sch @(NSC "addresses") @(AddressRev A2 B1) qp
    ]
  T.putStrLn "\n====== 5 ========\n"
  conn <- connectPostgreSQL "dbname=schema_test user=avia host=localhost"
  ar <- selectSch @Sch @(NSC "addresses") @(AddressRev A2 B1) conn qp
  print ar
  T.putStrLn "\n====== 8 ========\n"
  (cids,t) <- insertSch Sch (NSC "countries") conn countries
  T.putStrLn t
  mapM_ (print @(PgTagged "id" Int32)) cids
  d <- utctDay <$> getCurrentTime
  T.putStrLn "\n====== 10 ========\n"
  T.putStrLn $ I2.insertJSONText @AddressI @(PgTagged "id" Int32) Sch (NSC "addresses")
  T.putStrLn "\n====== 11 ========\n"
  let
    insData =
      [ MkAddressI (Just "street") Nothing (Just $ PgArr ["s","S12"]) (Just $ PgArr [1,2]) (SchList
        [ MkCustomerI "Ivan" $ SchList
          [ MkOrderI d "1" 1 (Just Order_state_paid) $ SchList
            [ MkOrdPosI 1 2 3 4, MkOrdPosI 2 3 4 5 ]
          , MkOrderI d "2a" 3 (Just Order_state_booked) $ SchList
            [ MkOrdPosI 3 2 3 4, MkOrdPosI 1 3 4 5.1 ] ]
        , MkCustomerI "Petr" $ SchList
          [ MkOrderI d "1v" 4 (Just Order_state_paid) $ SchList
            [ MkOrdPosI 1 2 3 4, MkOrdPosI 2 3 4 5 ]
          , MkOrderI d "xx" 5 (Just Order_state_delivered) $ SchList
            [ MkOrdPosI 5 6 3 4, MkOrdPosI 1 3 3 5.1 ] ] ]) mempty
      , MkAddressI Nothing (Just "zipcode") Nothing (Just $ PgArr [5,7]) mempty $ SchList [MkCompanyI "Typeable"]
      , MkAddressI (Just "street2") (Just "zip2") (Just mempty) Nothing (SchList [MkCustomerI "Dima" mempty])
        $ SchList [MkCompanyI "WellTyped"] ]
  as1 :: [PgTagged '["id", "cust_addr"] (Int32, SchList (PgTagged "id" Int32))]
    <- fmap fst $ I2.insertJSON @AddressI Sch (NSC "addresses") conn insData
  T.putStrLn "\n\n\n"
  T.putStrLn $ I2.insertJSONText_ @AddressI Sch (NSC "addresses")
  T.putStrLn "\n\n\n"
  void $ updateByCond_ @Sch @(NSC "addresses") conn
    (pgTag @"zipcode" (Just @Text "zip_new"))
    $ "street" =? Just @Text "street2"
  T.putStrLn "\n====== 20 ========\n"
  -- Prelude.putStrLn $ show as1
  selectSch @Sch @(NSC "countries") @Country conn qpEmpty >>= print
  T.putStrLn ""
  T.putStrLn "\n====== 22 ========\n"
  bitraverse T.putStrLn print $ selectText @Sch @(NSC "cities") @(City A1 B1) qpEmpty
  selectSch @Sch @(NSC "cities") @(City A1 B1) conn qpEmpty >>= print
  T.putStrLn ""
  bitraverse T.putStrLn print  $ selectText @Sch @(NSC "cities") @(City A2 B1) qpEmpty
  selectSch @Sch @(NSC "cities") @(City A2 B1) conn qpEmpty >>= print
  T.putStrLn ""
  T.putStrLn "\n====== 23 ========\n"
  selectSch @Sch @(NSC "addresses") @(Address A1 B1) conn qpEmpty >>= print
  T.putStrLn ""
  selectSch @Sch @(NSC "addresses") @(Address A2 B1) conn qp >>= print
  T.putStrLn ""
  T.putStrLn "\n====== 24 ========\n"
  selectSch @Sch @(NSC "addresses") @(AddressRev A1 B1) conn qp' >>= print
  selectSch @Sch @(NSC "addresses") @(AddressRev A2 B1) conn qp >>= print
  where
    qp = qpEmpty
      { qpConds =
        [rootCond
          (pparent (NSC "address_city")
            $ pparent (NSC "city_country") ("code" =? Just @Text "RU"))]
      , qpOrds = [ rootOrd [ascf "street"] ] }
    qp' = qp { qpLOs = [rootLO $ LO (Just 1) (Just 1)] }
