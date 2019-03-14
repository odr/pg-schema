{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Sch(Sch) where

-- This file is generated and can't be edited.

import GHC.Generics
import PgSchema


hashSchema :: Int
hashSchema = -7695617572686231947

data Sch


instance CTypDef Sch "date" where
  type TTypDef Sch "date" = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch "int4" where
  type TTypDef Sch "int4" = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch "numeric" where
  type TTypDef Sch "numeric" = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch "order_state" where
  type TTypDef Sch "order_state" = 
    'TypDef "E" 'Nothing '[ "paid","booked","delivered" ]

data instance PGEnum Sch "order_state" = 
  Order_state_paid | Order_state_booked | Order_state_delivered
  deriving (Show, Read, Ord, Eq, Generic)

instance CTypDef Sch "text" where
  type TTypDef Sch "text" = 
    'TypDef "S" 'Nothing '[  ]

instance CTypDef Sch "timestamptz" where
  type TTypDef Sch "timestamptz" = 
    'TypDef "D" 'Nothing '[  ]

instance CFldDef Sch "addresses" "app" where
  type TFldDef Sch "addresses" "app" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "addresses" "city_id" where
  type TFldDef Sch "addresses" "city_id" = 
    'FldDef "int4" 'True 'False

instance CFldDef Sch "addresses" "home" where
  type TFldDef Sch "addresses" "home" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "addresses" "id" where
  type TFldDef Sch "addresses" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "addresses" "street" where
  type TFldDef Sch "addresses" "street" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "addresses" "zipcode" where
  type TFldDef Sch "addresses" "zipcode" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "articles" "code" where
  type TFldDef Sch "articles" "code" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "articles" "created_at" where
  type TFldDef Sch "articles" "created_at" = 
    'FldDef "timestamptz" 'False 'True

instance CFldDef Sch "articles" "id" where
  type TFldDef Sch "articles" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "articles" "name" where
  type TFldDef Sch "articles" "name" = 
    'FldDef "text" 'False 'False

instance CFldDef Sch "articles" "updated_at" where
  type TFldDef Sch "articles" "updated_at" = 
    'FldDef "timestamptz" 'True 'False

instance CFldDef Sch "cities" "country_id" where
  type TFldDef Sch "cities" "country_id" = 
    'FldDef "int4" 'True 'False

instance CFldDef Sch "cities" "id" where
  type TFldDef Sch "cities" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "cities" "name" where
  type TFldDef Sch "cities" "name" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "companies" "address_id" where
  type TFldDef Sch "companies" "address_id" = 
    'FldDef "int4" 'True 'False

instance CFldDef Sch "companies" "created_at" where
  type TFldDef Sch "companies" "created_at" = 
    'FldDef "timestamptz" 'False 'True

instance CFldDef Sch "companies" "id" where
  type TFldDef Sch "companies" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "companies" "name" where
  type TFldDef Sch "companies" "name" = 
    'FldDef "text" 'False 'False

instance CFldDef Sch "companies" "updated_at" where
  type TFldDef Sch "companies" "updated_at" = 
    'FldDef "timestamptz" 'True 'False

instance CFldDef Sch "countries" "code" where
  type TFldDef Sch "countries" "code" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "countries" "id" where
  type TFldDef Sch "countries" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "countries" "name" where
  type TFldDef Sch "countries" "name" = 
    'FldDef "text" 'False 'False

instance CFldDef Sch "customers" "address_id" where
  type TFldDef Sch "customers" "address_id" = 
    'FldDef "int4" 'True 'False

instance CFldDef Sch "customers" "created_at" where
  type TFldDef Sch "customers" "created_at" = 
    'FldDef "timestamptz" 'False 'True

instance CFldDef Sch "customers" "id" where
  type TFldDef Sch "customers" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "customers" "name" where
  type TFldDef Sch "customers" "name" = 
    'FldDef "text" 'False 'False

instance CFldDef Sch "customers" "note" where
  type TFldDef Sch "customers" "note" = 
    'FldDef "text" 'True 'False

instance CFldDef Sch "customers" "updated_at" where
  type TFldDef Sch "customers" "updated_at" = 
    'FldDef "timestamptz" 'True 'False

instance CFldDef Sch "order_positions" "article_id" where
  type TFldDef Sch "order_positions" "article_id" = 
    'FldDef "int4" 'False 'False

instance CFldDef Sch "order_positions" "cnt" where
  type TFldDef Sch "order_positions" "cnt" = 
    'FldDef "int4" 'False 'False

instance CFldDef Sch "order_positions" "num" where
  type TFldDef Sch "order_positions" "num" = 
    'FldDef "int4" 'False 'False

instance CFldDef Sch "order_positions" "order_id" where
  type TFldDef Sch "order_positions" "order_id" = 
    'FldDef "int4" 'False 'False

instance CFldDef Sch "order_positions" "price" where
  type TFldDef Sch "order_positions" "price" = 
    'FldDef "numeric" 'False 'False

instance CFldDef Sch "orders" "created_at" where
  type TFldDef Sch "orders" "created_at" = 
    'FldDef "timestamptz" 'False 'True

instance CFldDef Sch "orders" "customer_id" where
  type TFldDef Sch "orders" "customer_id" = 
    'FldDef "int4" 'False 'False

instance CFldDef Sch "orders" "day" where
  type TFldDef Sch "orders" "day" = 
    'FldDef "date" 'False 'False

instance CFldDef Sch "orders" "id" where
  type TFldDef Sch "orders" "id" = 
    'FldDef "int4" 'False 'True

instance CFldDef Sch "orders" "num" where
  type TFldDef Sch "orders" "num" = 
    'FldDef "text" 'False 'False

instance CFldDef Sch "orders" "seller_id" where
  type TFldDef Sch "orders" "seller_id" = 
    'FldDef "int4" 'False 'False

instance CFldDef Sch "orders" "state" where
  type TFldDef Sch "orders" "state" = 
    'FldDef "order_state" 'True 'False

instance CFldDef Sch "orders" "trader_id" where
  type TFldDef Sch "orders" "trader_id" = 
    'FldDef "int4" 'True 'False

instance CFldDef Sch "orders" "updated_at" where
  type TFldDef Sch "orders" "updated_at" = 
    'FldDef "timestamptz" 'True 'False

instance CTabDef Sch "addresses" where
  type TTabDef Sch "addresses" = 
    'TabDef '[ "id","city_id","street","home","app","zipcode" ] '[ "id" ] '[  ]

instance CTabDef Sch "articles" where
  type TTabDef Sch "articles" = 
    'TabDef '[ "id","name","code","created_at","updated_at" ] '[ "id" ] '[ '[ "name" ] ]

instance CTabDef Sch "cities" where
  type TTabDef Sch "cities" = 
    'TabDef '[ "id","country_id","name" ] '[ "id" ] '[  ]

instance CTabDef Sch "companies" where
  type TTabDef Sch "companies" = 
    'TabDef '[ "id","name","address_id","created_at","updated_at" ] '[ "id" ] '[  ]

instance CTabDef Sch "countries" where
  type TTabDef Sch "countries" = 
    'TabDef '[ "id","name","code" ] '[ "id" ] '[  ]

instance CTabDef Sch "customers" where
  type TTabDef Sch "customers" = 
    'TabDef '[ "id","name","address_id","note","created_at","updated_at" ] '[ "id" ] '[  ]

instance CTabDef Sch "order_positions" where
  type TTabDef Sch "order_positions" = 
    'TabDef '[ "order_id","num","article_id","cnt","price" ] '[ "order_id","num" ] '[ '[ "order_id","article_id" ] ]

instance CTabDef Sch "orders" where
  type TTabDef Sch "orders" = 
    'TabDef '[ "id","day","num","customer_id","seller_id","trader_id","state","created_at","updated_at" ] '[ "id" ] '[  ]

instance CRelDef Sch "address_city" where
  type TRelDef Sch "address_city" = 
    'RelDef "addresses" "cities" '[ '( "city_id","id" ) ]

instance CRelDef Sch "city_country" where
  type TRelDef Sch "city_country" = 
    'RelDef "cities" "countries" '[ '( "country_id","id" ) ]

instance CRelDef Sch "comp_addr" where
  type TRelDef Sch "comp_addr" = 
    'RelDef "companies" "addresses" '[ '( "address_id","id" ) ]

instance CRelDef Sch "cust_addr" where
  type TRelDef Sch "cust_addr" = 
    'RelDef "customers" "addresses" '[ '( "address_id","id" ) ]

instance CRelDef Sch "opos_article" where
  type TRelDef Sch "opos_article" = 
    'RelDef "order_positions" "articles" '[ '( "article_id","id" ) ]

instance CRelDef Sch "opos_order" where
  type TRelDef Sch "opos_order" = 
    'RelDef "order_positions" "orders" '[ '( "order_id","id" ) ]

instance CRelDef Sch "ord_cust" where
  type TRelDef Sch "ord_cust" = 
    'RelDef "orders" "customers" '[ '( "customer_id","id" ) ]

instance CRelDef Sch "ord_seller" where
  type TRelDef Sch "ord_seller" = 
    'RelDef "orders" "companies" '[ '( "seller_id","id" ) ]

instance CRelDef Sch "ord_trader" where
  type TRelDef Sch "ord_trader" = 
    'RelDef "orders" "companies" '[ '( "trader_id","id" ) ]

instance CSchema Sch where
  type TSchema Sch = "sch"
  type TTabs Sch = '[ "addresses","articles","cities","companies","countries","customers","order_positions","orders" ]
  type TRels Sch = '[ "address_city","city_country","comp_addr","cust_addr","opos_article","opos_order","ord_cust","ord_seller","ord_trader" ]
  type TTypes Sch = '[ "date","int4","numeric","order_state","text","timestamptz" ]
