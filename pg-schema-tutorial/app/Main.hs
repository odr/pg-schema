{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeAbstractions #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Bifunctor
import Data.Bitraversable
import Data.Fixed
import Data.Functor
import Data.List as L
import Data.Singletons
import Data.Tagged
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
import GHC.TypeLits
import PgSchema
import Database.PostgreSQL.DML.InsertJSON qualified as I2
import Database.PostgreSQL.DML.Update
import Sch
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Instances ()
import Database.Types.EmptyField (EmptyField)
import Prelude as P

type NSC name = "sch" ->> name

data RenamerSch

type family TRenamerSch (s :: Symbol) :: Symbol where
  TRenamerSch "minPrice" = "price"
  TRenamerSch "maxPrice" = "price"
  TRenamerSch "sumPrice" = "price"
  TRenamerSch "avgPrice" = "price"
  TRenamerSch s = s

instance Renamer RenamerSch where
  type Apply RenamerSch s = TRenamerSch s

data Country = MkCountry
  { code :: Maybe Text
  , name :: Text }
  -- TODO: cycle references lead to halt! Should check to avoid it
  -- , city_country :: [City A2 B1] }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Country where
  arbitrary = genericArbitrarySingle

data A = A1 | A2
data B = B1 | B2

data Address (a::A) (b::B) = MkAddress
  { street  :: Text
  , home    :: If (a == A1) (Maybe Text) EmptyField
  , app     :: Maybe Text
  , zipcode :: Maybe Text
  , phones  :: Maybe (PgArr Text)
  , numbers :: Maybe (PgArr Int32) }
  deriving Generic

deriving instance Show (Address A2 B1)
deriving instance Show (Address A1 B1)

data City a b = MkCity
  { name        :: Maybe Text
  , city_country :: If (a == A1) EmptyField (Maybe Country)
  , address_city :: [Address a b] }
  deriving Generic

deriving instance Show (City A2 B1)
deriving instance Show (City A1 B1)

data AddressRev a b = MkAddressRev
  { street       :: Text
  , home         :: Maybe Text
  , app          :: If (a == A1) (Maybe Text) EmptyField
  , zipcode      :: Maybe Text
  , address_city :: City a b }
  deriving Generic

deriving instance Show (AddressRev A2 B1)
deriving instance Show (AddressRev A1 B1)

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

data PosCnt = MkPosCnt
  { order_id  :: Int32
  , cnt       :: Aggr "count" Int64
  , minPrice  :: Aggr "min" (Maybe Centi)
  , maxPrice  :: Aggr "max" (Maybe Centi)
  , sumPrice  :: Aggr "sum" (Maybe Double)
  , avgPrice  :: Aggr "avg" (Maybe Double) }
  deriving Generic

-- data Customer = Customer
--   { }
data Order = MkOrder
  { day        :: Day
  , num        :: Text
  , ord_seller :: Company
  , opos_order :: [OrdPos]
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
  , opos_order :: [OrdPosI] }
  deriving Generic

data CustomerI = MkCustomerI
  { name :: Text
  , ord_cust :: [OrderI]
  }
  deriving Generic

newtype CompanyI = MkCompanyI
  { name :: Text }
  deriving Generic

data AddressI = MkAddressI
  { street :: Text
  , zipcode :: Maybe Text
  , phones :: Maybe (PgArr Text)
  , numbers :: Maybe (PgArr Int32)
  , cust_addr :: [CustomerI]
  , comp_addr :: [CompanyI] }
  deriving Generic

instance HasResolution a => FromField (Fixed a) where
  fromField f mb = (realToFrac :: Rational -> Fixed a) <$> fromField f mb

instance HasResolution a => ToField (Fixed a) where
  toField = toField . (realToFrac :: Fixed a -> Double)

newtype CustId = CustId { id :: Int32 }
  deriving Generic

data AddressRet = AddressRet
  { id :: Int32
  , cust_addr :: ["id" := Int32]}
  deriving Generic

type HSch s r = HListTag (HListTagRep RenamerSch Sch (NSC s) r)

selSch :: forall (tn :: Symbol) -> forall r h.
  ( IsoHListTag RenamerSch Sch (NSC tn) r, h ~ HSch tn r
  , CHListInfo Sch (NSC tn) h, FromRow h )
  => Connection -> QueryParam Sch (NSC tn) -> IO ([r], (Text,[SomeToField]))
selSch tn = selectSch RenamerSch Sch (NSC tn)

insSch
  :: forall tn -> forall r r' h h'
  . InsertReturning' RenamerSch Sch (NSC tn) r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
insSch tn = insertSch RenamerSch Sch (NSC tn)

insSch_
  :: forall tn -> forall r h
  . InsertNonReturning' RenamerSch Sch (NSC tn) r h
  => Connection -> [r] -> IO (Int64, Text)
insSch_ tn = insertSch_ RenamerSch Sch (NSC tn)

selSchText :: forall tn -> forall r h. (CHListInfo Sch (NSC tn) h, h ~ HSch tn r) =>
  QueryParam Sch (NSC tn) -> (Text,[SomeToField])
selSchText tn @r = selectText @Sch @(NSC tn) @(HSch tn r)

insJSONText :: forall tn -> forall r r' h h'.
  ( SrcJSON RenamerSch Sch (NSC tn) r h, TgtJSON RenamerSch Sch (NSC tn) r' h') => Text
insJSONText tn @r @r' @h @h' = insertJSONText RenamerSch Sch (NSC tn) @r @r' @h @h'

insJSON_
  :: forall tn -> forall r h. InsertNonReturning RenamerSch Sch (NSC tn) r h
  => Connection -> [r] -> IO Text
insJSON_ tn = insertJSON_ RenamerSch Sch (NSC tn)

insJSON
  :: forall tn -> forall r r' h h'. InsertReturning RenamerSch Sch (NSC tn) r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
insJSON tn = insertJSON RenamerSch Sch (NSC tn)

upsJSON_
  :: forall tn -> forall r h. UpsertNonReturning RenamerSch Sch (NSC tn) r h
  => Connection -> [r] -> IO Text
upsJSON_ tn = upsertJSON_ RenamerSch Sch (NSC tn)

upsJSON
  :: forall tn -> forall r r' h h'. UpsertReturning RenamerSch Sch (NSC tn) r r' h h'
  => Connection -> [r] -> IO ([r'], Text)
upsJSON tn = upsertJSON RenamerSch Sch (NSC tn)

updByCond_
  :: forall tn -> forall r h.
  ( h ~ HRep RenamerSch Sch (NSC tn) r, HListInfo RenamerSch Sch (NSC tn) r h
  , ToRow h, AllPlain Sch (NSC tn) h )
  => Connection -> r -> Cond Sch (NSC tn) -> IO Int64
updByCond_ tn = updateByCond_ RenamerSch Sch (NSC tn)

updByCond :: forall tn -> forall r r' h h'.
  ( UpdateReturning RenamerSch Sch (NSC tn) r r' h h'
  , AllPlain Sch (NSC tn) h, ToRow h, FromRow h' )
  => Connection -> r -> Cond Sch (NSC tn) -> IO [r']
updByCond tn = updateByCond RenamerSch Sch (NSC tn)

main :: IO ()
main = do
  countries <- generate $ replicateM 5 (arbitrary @Country)
  mapM_ (\(a,b) -> T.putStrLn a >> print b)
    [ selSchText "countries" @Country qpEmpty
    , selSchText "articles" @Article qpEmpty
    , selSchText "cities" @(City A1 B1) qpEmpty
    , selSchText "cities" @(City A2 B1) qpEmpty
    , selSchText "addresses" @(Address A1 B1) qpEmpty
    , selSchText "addresses" @(Address A2 B1) qp
    , selSchText "addresses" @(AddressRev A1 B1) qp
    , selSchText "addresses" @(AddressRev A2 B1) qp
    , selSchText "order_positions" @PosCnt qpEmpty
    , selSchText "order_positions" @("_cnt" := Aggr "count" Int64) qpEmpty
    , selSchText "order_positions" @("_cnt" := Aggr' "count" Int64) qpEmpty
    , selSchText "order_positions" @("cnt" := Aggr "count" Int64) qpEmpty
    , selSchText "order_positions" @("cnt" := Aggr' "count" Int64) qpEmpty
    , selSchText "order_positions" @("cnt" := Aggr' "max" Int32 :. "cnt" := Aggr' "count" Int64) qpEmpty
    , selSchText "order_positions" @("cnt" := Aggr "max" (Maybe Int32)) qpEmpty
    ]
  T.putStrLn "\n====== 5 ========\n"
  conn <- connectPostgreSQL "dbname=schema_test user=avia host=localhost"
  (_::[Article], _) <- selSch "articles" conn qpEmpty
  -- upsJSON_ "addresses" @(AddressRev A2 B1) conn [MkAddressRev
  --   { street = "str1", home = Nothing, app = emptyField, zipcode = Nothing
  --   , address_city = Nothing }]
  ar <- selSch "addresses" @(AddressRev A2 B1) conn qp
  mapM_ print ar
  T.putStrLn "\n====== 8 ========\n"
  (cids,t) <- insSch "countries" conn countries
  T.putStrLn t
  mapM_ (print @("id" := Int32)) cids
  d <- utctDay <$> getCurrentTime
  T.putStrLn "\n====== 10 ========\n"
  T.putStrLn $ insJSONText "addresses" @AddressI @("id" := Int32)
  T.putStrLn "\n====== 11 ========\n"
  let
    insData =
      [ MkAddressI "street" Nothing (Just $ PgArr ["s","S12"]) (Just $ PgArr [1,2])
        [ MkCustomerI "Ivan"
          [ MkOrderI d "1" 1 (Just Order_state_paid)
            [ MkOrdPosI 1 2 3 4, MkOrdPosI 2 3 4 5 ]
          , MkOrderI d "2a" 3 (Just Order_state_booked)
            [ MkOrdPosI 3 2 3 4, MkOrdPosI 1 3 4 5.1 ] ]
        , MkCustomerI "Petr"
          [ MkOrderI d "1v" 4 (Just Order_state_paid)
            [ MkOrdPosI 1 2 3 4, MkOrdPosI 2 3 4 5 ]
          , MkOrderI d "xx" 5 (Just Order_state_delivered)
            [ MkOrdPosI 5 6 3 4, MkOrdPosI 1 3 3 5.1 ] ] ] mempty
      , MkAddressI "str" (Just "zipcode") mempty (Just $ PgArr [5,7]) mempty  [MkCompanyI "Typeable"]
      , MkAddressI "street2" (Just "zip2") mempty Nothing [MkCustomerI "Dima" mempty]
        [MkCompanyI "WellTyped"] ]
  void $ insJSON_ "addresses" @AddressI conn insData
  (as1 :: ["id" := Int32 :. "cust_addr" := ["id" := Int32 :. "name" := Text]], _insTxt)
    <- insJSON "addresses" @AddressI conn insData
  curTime <- T.show <$> getCurrentTime
  upsJSON_ "addresses" conn $ as1 <&> \(a :. Tagged xs) ->
    a :. "cust_addr" =: (xs <&> \(cid :. cname) ->
      cid :. fmap (<> ": " <> curTime <> " updated") cname)
  let
    upsVals = as1 <&> \(a :. Tagged xs) -> a :. "cust_addr" =:
      (xs <&> \(cid :. _) -> cid :. "note" =: Just curTime)
  mapM_ print upsVals
  upsJSON_ "addresses" conn upsVals
  T.putStrLn "\n====== 13 ========\n"
  (as2 :: ["id" := Int32 :. "cust_addr" :=
    ["id" := Int32 :. "updated_at" := Maybe ZonedTime]], _upsTxt)
    <- upsJSON "addresses" conn upsVals
  mapM_ print as2
  T.putStrLn "\n====== 15 ========\n"
  void $ updByCond_ "addresses" conn ("zipcode" =: Just @Text "zip_new")
    $ "street" =? ("street2"::Text)
  (xs :: ["zipcode" := Maybe Text :. "phones" := Maybe (PgArr Text)]) <-
    updByCond "addresses" conn
      ("phones" =: Just (PgArr ["111" :: Text,"222"]))
      $ "street" =? ("street2"::Text)
  mapM_ print xs
  T.putStrLn "\n====== 20 ========\n"
  -- Prelude.putStrLn $ show as1
  selSch "countries" @Country conn qpEmpty >>= print
  T.putStrLn ""
  T.putStrLn "\n====== 22 ========\n"
  bitraverse T.putStrLn print $ selSchText "cities" @(City A1 B1) qpEmpty
  selSch "cities" @(City A1 B1) conn qpEmpty >>= print
  T.putStrLn ""
  bitraverse T.putStrLn print  $ selSchText "cities" @(City A2 B1) qpEmpty
  selSch "cities" @(City A2 B1) conn qpEmpty >>= print
  T.putStrLn ""
  T.putStrLn "\n====== 23 ========\n"
  selSch "addresses" @(Address A1 B1) conn qpEmpty >>= print
  T.putStrLn ""
  selSch "addresses" @(Address A2 B1) conn qp >>= print
  T.putStrLn ""
  T.putStrLn "\n====== 24 ========\n"
  selSch "addresses" @(AddressRev A1 B1) conn qp >>= print
  selSch "addresses" @(AddressRev A2 B1) conn qp' >>= print
  where
    qp = qRoot qpr
    qpr = do
      qDistinct
      -- qDistinctOn [ascf "street"]
      qWhere
        $ pparent (NSC "address_city")
        $ pparent (NSC "city_country")
        $ "code" =? Just @Text "RU"
    qp' = qRoot do
      qpr
      qLimit 5
      qOffset 0
      qPath "address_city" do
        -- qDistinct -- not work (reason: RelOne)
        qWhere $ "name" =? Just @Text "street"
        qPath "address_city" do
          qLimit 2
          qDistinctOn [descf "street"]
        qPath "city_country" do
          qDistinctOn [descf "name"]
          -- qLimit 3 -- not work (reason: RelOne)
          qOrderBy [ascf "name"]
          qWhere $ "name" >? ("Bar" :: Text)
        qDistinctOn [ascf "name"]
