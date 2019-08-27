{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Sch(Sch) where

-- This file is generated and can't be edited.

import GHC.Generics
import PgSchema


hashSchema :: Int
hashSchema = -4230393316157406418

data Sch

instance CTypDef Sch ( 'NameNS "pg_catalog" "date" ) where
  type TTypDef Sch ( 'NameNS "pg_catalog" "date" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( 'NameNS "pg_catalog" "int4" ) where
  type TTypDef Sch ( 'NameNS "pg_catalog" "int4" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( 'NameNS "pg_catalog" "numeric" ) where
  type TTypDef Sch ( 'NameNS "pg_catalog" "numeric" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( 'NameNS "pg_catalog" "text" ) where
  type TTypDef Sch ( 'NameNS "pg_catalog" "text" ) = 
    'TypDef "S" 'Nothing '[  ]

instance CTypDef Sch ( 'NameNS "pg_catalog" "timestamptz" ) where
  type TTypDef Sch ( 'NameNS "pg_catalog" "timestamptz" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( 'NameNS "sch" "order_state" ) where
  type TTypDef Sch ( 'NameNS "sch" "order_state" ) = 
    'TypDef "E" 'Nothing '[ "paid","booked","delivered" ]

data instance PGEnum Sch ( 'NameNS "sch" "order_state" ) = 
  Order_state_paid | Order_state_booked | Order_state_delivered
  deriving (Show, Read, Ord, Eq, Generic)
instance CFldDef Sch ( 'NameNS "sch" "addresses" ) "app" where
  type TFldDef Sch ( 'NameNS "sch" "addresses" ) "app" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "addresses" ) "city_id" where
  type TFldDef Sch ( 'NameNS "sch" "addresses" ) "city_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "addresses" ) "home" where
  type TFldDef Sch ( 'NameNS "sch" "addresses" ) "home" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "addresses" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "addresses" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "addresses" ) "street" where
  type TFldDef Sch ( 'NameNS "sch" "addresses" ) "street" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "addresses" ) "zipcode" where
  type TFldDef Sch ( 'NameNS "sch" "addresses" ) "zipcode" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "articles" ) "code" where
  type TFldDef Sch ( 'NameNS "sch" "articles" ) "code" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "articles" ) "created_at" where
  type TFldDef Sch ( 'NameNS "sch" "articles" ) "created_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "articles" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "articles" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "articles" ) "name" where
  type TFldDef Sch ( 'NameNS "sch" "articles" ) "name" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "articles" ) "updated_at" where
  type TFldDef Sch ( 'NameNS "sch" "articles" ) "updated_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "cities" ) "country_id" where
  type TFldDef Sch ( 'NameNS "sch" "cities" ) "country_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "cities" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "cities" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "cities" ) "name" where
  type TFldDef Sch ( 'NameNS "sch" "cities" ) "name" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "companies" ) "address_id" where
  type TFldDef Sch ( 'NameNS "sch" "companies" ) "address_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "companies" ) "created_at" where
  type TFldDef Sch ( 'NameNS "sch" "companies" ) "created_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "companies" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "companies" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "companies" ) "name" where
  type TFldDef Sch ( 'NameNS "sch" "companies" ) "name" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "companies" ) "updated_at" where
  type TFldDef Sch ( 'NameNS "sch" "companies" ) "updated_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "countries" ) "code" where
  type TFldDef Sch ( 'NameNS "sch" "countries" ) "code" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "countries" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "countries" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "countries" ) "name" where
  type TFldDef Sch ( 'NameNS "sch" "countries" ) "name" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "customers" ) "address_id" where
  type TFldDef Sch ( 'NameNS "sch" "customers" ) "address_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "customers" ) "created_at" where
  type TFldDef Sch ( 'NameNS "sch" "customers" ) "created_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "customers" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "customers" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "customers" ) "name" where
  type TFldDef Sch ( 'NameNS "sch" "customers" ) "name" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "customers" ) "note" where
  type TFldDef Sch ( 'NameNS "sch" "customers" ) "note" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "customers" ) "updated_at" where
  type TFldDef Sch ( 'NameNS "sch" "customers" ) "updated_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "order_positions" ) "article_id" where
  type TFldDef Sch ( 'NameNS "sch" "order_positions" ) "article_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "order_positions" ) "cnt" where
  type TFldDef Sch ( 'NameNS "sch" "order_positions" ) "cnt" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "order_positions" ) "num" where
  type TFldDef Sch ( 'NameNS "sch" "order_positions" ) "num" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "order_positions" ) "order_id" where
  type TFldDef Sch ( 'NameNS "sch" "order_positions" ) "order_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "order_positions" ) "price" where
  type TFldDef Sch ( 'NameNS "sch" "order_positions" ) "price" = 
    'FldDef ( 'NameNS "pg_catalog" "numeric" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "created_at" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "created_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "customer_id" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "customer_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "day" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "day" = 
    'FldDef ( 'NameNS "pg_catalog" "date" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "id" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'True

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "num" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "num" = 
    'FldDef ( 'NameNS "pg_catalog" "text" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "seller_id" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "seller_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'False 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "state" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "state" = 
    'FldDef ( 'NameNS "sch" "order_state" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "trader_id" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "trader_id" = 
    'FldDef ( 'NameNS "pg_catalog" "int4" ) 'True 'False

instance CFldDef Sch ( 'NameNS "sch" "orders" ) "updated_at" where
  type TFldDef Sch ( 'NameNS "sch" "orders" ) "updated_at" = 
    'FldDef ( 'NameNS "pg_catalog" "timestamptz" ) 'True 'False

instance CTabDef Sch ( 'NameNS "sch" "addresses" ) where
  type TTabDef Sch ( 'NameNS "sch" "addresses" ) = 
    'TabDef '[ "id","city_id","street","home","app","zipcode" ] '[ "id" ] '[  ]

instance CTabDef Sch ( 'NameNS "sch" "articles" ) where
  type TTabDef Sch ( 'NameNS "sch" "articles" ) = 
    'TabDef '[ "id"
      ,"name","code","created_at","updated_at" ] '[ "id" ] '[ '[ "name" ] ]

instance CTabDef Sch ( 'NameNS "sch" "cities" ) where
  type TTabDef Sch ( 'NameNS "sch" "cities" ) = 
    'TabDef '[ "id","country_id","name" ] '[ "id" ] '[  ]

instance CTabDef Sch ( 'NameNS "sch" "companies" ) where
  type TTabDef Sch ( 'NameNS "sch" "companies" ) = 
    'TabDef '[ "id"
      ,"name","address_id","created_at","updated_at" ] '[ "id" ] '[  ]

instance CTabDef Sch ( 'NameNS "sch" "countries" ) where
  type TTabDef Sch ( 'NameNS "sch" "countries" ) = 
    'TabDef '[ "id","name","code" ] '[ "id" ] '[  ]

instance CTabDef Sch ( 'NameNS "sch" "customers" ) where
  type TTabDef Sch ( 'NameNS "sch" "customers" ) = 
    'TabDef '[ "id"
      ,"name","address_id","note","created_at","updated_at" ] '[ "id" ] '[  ]

instance CTabDef Sch ( 'NameNS "sch" "order_positions" ) where
  type TTabDef Sch ( 'NameNS "sch" "order_positions" ) = 
    'TabDef '[ "order_id","num","article_id"
      ,"cnt","price" ] '[ "order_id","num" ] '[ '[ "order_id","article_id" ] ]

instance CTabDef Sch ( 'NameNS "sch" "orders" ) where
  type TTabDef Sch ( 'NameNS "sch" "orders" ) = 
    'TabDef '[ "id","day","num","customer_id","seller_id"
      ,"trader_id","state","created_at","updated_at" ] '[ "id" ] '[  ]

instance CRelDef Sch ( 'NameNS "sch" "address_city" ) where
  type TRelDef Sch ( 'NameNS "sch" "address_city" ) = 
    'RelDef ( 'NameNS "sch" "addresses" ) ( 'NameNS "sch" "cities" ) '[ '( "city_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "city_country" ) where
  type TRelDef Sch ( 'NameNS "sch" "city_country" ) = 
    'RelDef ( 'NameNS "sch" "cities" ) ( 'NameNS "sch" "countries" ) '[ '( "country_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "comp_addr" ) where
  type TRelDef Sch ( 'NameNS "sch" "comp_addr" ) = 
    'RelDef ( 'NameNS "sch" "companies" ) ( 'NameNS "sch" "addresses" ) '[ '( "address_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "cust_addr" ) where
  type TRelDef Sch ( 'NameNS "sch" "cust_addr" ) = 
    'RelDef ( 'NameNS "sch" "customers" ) ( 'NameNS "sch" "addresses" ) '[ '( "address_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "opos_article" ) where
  type TRelDef Sch ( 'NameNS "sch" "opos_article" ) = 
    'RelDef ( 'NameNS "sch" "order_positions" ) ( 'NameNS "sch" "articles" ) '[ '( "article_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "opos_order" ) where
  type TRelDef Sch ( 'NameNS "sch" "opos_order" ) = 
    'RelDef ( 'NameNS "sch" "order_positions" ) ( 'NameNS "sch" "orders" ) '[ '( "order_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "ord_cust" ) where
  type TRelDef Sch ( 'NameNS "sch" "ord_cust" ) = 
    'RelDef ( 'NameNS "sch" "orders" ) ( 'NameNS "sch" "customers" ) '[ '( "customer_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "ord_seller" ) where
  type TRelDef Sch ( 'NameNS "sch" "ord_seller" ) = 
    'RelDef ( 'NameNS "sch" "orders" ) ( 'NameNS "sch" "companies" ) '[ '( "seller_id"
      ,"id" ) ]

instance CRelDef Sch ( 'NameNS "sch" "ord_trader" ) where
  type TRelDef Sch ( 'NameNS "sch" "ord_trader" ) = 
    'RelDef ( 'NameNS "sch" "orders" ) ( 'NameNS "sch" "companies" ) '[ '( "trader_id"
      ,"id" ) ]

instance CSchema Sch where
  type TTabs Sch = '[ ( 'NameNS "sch" "addresses" ),( 'NameNS "sch" "articles" )
    ,( 'NameNS "sch" "cities" ),( 'NameNS "sch" "companies" )
    ,( 'NameNS "sch" "countries" ),( 'NameNS "sch" "customers" )
    ,( 'NameNS "sch" "order_positions" ),( 'NameNS "sch" "orders" ) ]

  type TRels Sch = '[ ( 'NameNS "sch" "address_city" )
    ,( 'NameNS "sch" "city_country" ),( 'NameNS "sch" "comp_addr" )
    ,( 'NameNS "sch" "cust_addr" ),( 'NameNS "sch" "opos_article" )
    ,( 'NameNS "sch" "opos_order" ),( 'NameNS "sch" "ord_cust" )
    ,( 'NameNS "sch" "ord_seller" ),( 'NameNS "sch" "ord_trader" ) ]

  type TTypes Sch = '[ ( 'NameNS "pg_catalog" "date" )
    ,( 'NameNS "pg_catalog" "int4" ),( 'NameNS "pg_catalog" "numeric" )
    ,( 'NameNS "pg_catalog" "text" ),( 'NameNS "pg_catalog" "timestamptz" )
    ,( 'NameNS "sch" "order_state" ) ]

