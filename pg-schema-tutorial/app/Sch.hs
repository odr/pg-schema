{- HLINT ignore -}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -freduction-depth=300 #-}
module Sch where

-- This file is generated and can't be edited.

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import PgSchema


data Sch

instance CTypDef Sch ( "pg_catalog" ->> "_int4" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_int4" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "int4" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_text" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_text" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "text" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "date" ) where
  type TTypDef Sch ( "pg_catalog" ->> "date" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "float8" ) where
  type TTypDef Sch ( "pg_catalog" ->> "float8" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "int4" ) where
  type TTypDef Sch ( "pg_catalog" ->> "int4" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "int8" ) where
  type TTypDef Sch ( "pg_catalog" ->> "int8" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "numeric" ) where
  type TTypDef Sch ( "pg_catalog" ->> "numeric" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "text" ) where
  type TTypDef Sch ( "pg_catalog" ->> "text" ) = 
    'TypDef "S" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "timestamptz" ) where
  type TTypDef Sch ( "pg_catalog" ->> "timestamptz" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( "sch" ->> "order_state" ) where
  type TTypDef Sch ( "sch" ->> "order_state" ) = 
    'TypDef "E" 'Nothing '[ "paid","booked","delivered" ]

data instance PGEnum Sch ( "sch" ->> "order_state" )
  = Order_state_paid | Order_state_booked | Order_state_delivered
  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)

instance Hashable (PGEnum Sch ( "sch" ->> "order_state" ))

instance NFData (PGEnum Sch ( "sch" ->> "order_state" ))

instance CFldDef Sch ( "sch" ->> "addresses" ) "app" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "app" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "addresses" ) "city_id" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "city_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "addresses" ) "home" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "home" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "addresses" ) "id" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "addresses" ) "numbers" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "numbers" = 
    'FldDef ( "pg_catalog" ->> "_int4" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "addresses" ) "phones" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "phones" = 
    'FldDef ( "pg_catalog" ->> "_text" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "addresses" ) "street" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "street" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "addresses" ) "zipcode" where
  type TFldDef Sch ( "sch" ->> "addresses" ) "zipcode" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "articles" ) "code" where
  type TFldDef Sch ( "sch" ->> "articles" ) "code" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "articles" ) "created_at" where
  type TFldDef Sch ( "sch" ->> "articles" ) "created_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "articles" ) "id" where
  type TFldDef Sch ( "sch" ->> "articles" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "articles" ) "name" where
  type TFldDef Sch ( "sch" ->> "articles" ) "name" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "articles" ) "updated_at" where
  type TFldDef Sch ( "sch" ->> "articles" ) "updated_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "cities" ) "country_id" where
  type TFldDef Sch ( "sch" ->> "cities" ) "country_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "cities" ) "id" where
  type TFldDef Sch ( "sch" ->> "cities" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "cities" ) "name" where
  type TFldDef Sch ( "sch" ->> "cities" ) "name" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "companies" ) "address_id" where
  type TFldDef Sch ( "sch" ->> "companies" ) "address_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "companies" ) "created_at" where
  type TFldDef Sch ( "sch" ->> "companies" ) "created_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "companies" ) "id" where
  type TFldDef Sch ( "sch" ->> "companies" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "companies" ) "name" where
  type TFldDef Sch ( "sch" ->> "companies" ) "name" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "companies" ) "updated_at" where
  type TFldDef Sch ( "sch" ->> "companies" ) "updated_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "countries" ) "code" where
  type TFldDef Sch ( "sch" ->> "countries" ) "code" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "countries" ) "id" where
  type TFldDef Sch ( "sch" ->> "countries" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "countries" ) "name" where
  type TFldDef Sch ( "sch" ->> "countries" ) "name" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "customers" ) "address_id" where
  type TFldDef Sch ( "sch" ->> "customers" ) "address_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "customers" ) "created_at" where
  type TFldDef Sch ( "sch" ->> "customers" ) "created_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "customers" ) "id" where
  type TFldDef Sch ( "sch" ->> "customers" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "customers" ) "name" where
  type TFldDef Sch ( "sch" ->> "customers" ) "name" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "customers" ) "note" where
  type TFldDef Sch ( "sch" ->> "customers" ) "note" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "customers" ) "updated_at" where
  type TFldDef Sch ( "sch" ->> "customers" ) "updated_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "order_positions" ) "article_id" where
  type TFldDef Sch ( "sch" ->> "order_positions" ) "article_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "order_positions" ) "cnt" where
  type TFldDef Sch ( "sch" ->> "order_positions" ) "cnt" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "order_positions" ) "num" where
  type TFldDef Sch ( "sch" ->> "order_positions" ) "num" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "order_positions" ) "order_id" where
  type TFldDef Sch ( "sch" ->> "order_positions" ) "order_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "order_positions" ) "price" where
  type TFldDef Sch ( "sch" ->> "order_positions" ) "price" = 
    'FldDef ( "pg_catalog" ->> "numeric" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "created_at" where
  type TFldDef Sch ( "sch" ->> "orders" ) "created_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "orders" ) "customer_id" where
  type TFldDef Sch ( "sch" ->> "orders" ) "customer_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "day" where
  type TFldDef Sch ( "sch" ->> "orders" ) "day" = 
    'FldDef ( "pg_catalog" ->> "date" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "id" where
  type TFldDef Sch ( "sch" ->> "orders" ) "id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'True

instance CFldDef Sch ( "sch" ->> "orders" ) "num" where
  type TFldDef Sch ( "sch" ->> "orders" ) "num" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "seller_id" where
  type TFldDef Sch ( "sch" ->> "orders" ) "seller_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'False 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "state" where
  type TFldDef Sch ( "sch" ->> "orders" ) "state" = 
    'FldDef ( "sch" ->> "order_state" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "trader_id" where
  type TFldDef Sch ( "sch" ->> "orders" ) "trader_id" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'True 'False

instance CFldDef Sch ( "sch" ->> "orders" ) "updated_at" where
  type TFldDef Sch ( "sch" ->> "orders" ) "updated_at" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False

instance CTabDef Sch ( "sch" ->> "addresses" ) where
  type TTabDef Sch ( "sch" ->> "addresses" ) = 
    'TabDef '[ "id","city_id"
      ,"street","home","app","zipcode","phones","numbers" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "sch" ->> "articles" ) where
  type TTabDef Sch ( "sch" ->> "articles" ) = 
    'TabDef '[ "id"
      ,"name","code","created_at","updated_at" ] '[ "id" ] '[ '[ "name" ] ]

instance CTabDef Sch ( "sch" ->> "cities" ) where
  type TTabDef Sch ( "sch" ->> "cities" ) = 
    'TabDef '[ "id","country_id","name" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "sch" ->> "companies" ) where
  type TTabDef Sch ( "sch" ->> "companies" ) = 
    'TabDef '[ "id"
      ,"name","address_id","created_at","updated_at" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "sch" ->> "countries" ) where
  type TTabDef Sch ( "sch" ->> "countries" ) = 
    'TabDef '[ "id","name","code" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "sch" ->> "customers" ) where
  type TTabDef Sch ( "sch" ->> "customers" ) = 
    'TabDef '[ "id"
      ,"name","address_id","note","created_at","updated_at" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "sch" ->> "order_positions" ) where
  type TTabDef Sch ( "sch" ->> "order_positions" ) = 
    'TabDef '[ "order_id","num","article_id"
      ,"cnt","price" ] '[ "order_id","num" ] '[ '[ "order_id","article_id" ] ]

instance CTabDef Sch ( "sch" ->> "orders" ) where
  type TTabDef Sch ( "sch" ->> "orders" ) = 
    'TabDef '[ "id","day","num","customer_id","seller_id"
      ,"trader_id","state","created_at","updated_at" ] '[ "id" ] '[  ]

instance CRelDef Sch ( "sch" ->> "address_city" ) where
  type TRelDef Sch ( "sch" ->> "address_city" ) = 
    'RelDef ( "sch" ->> "addresses" ) ( "sch" ->> "cities" ) '[ '( "city_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "city_country" ) where
  type TRelDef Sch ( "sch" ->> "city_country" ) = 
    'RelDef ( "sch" ->> "cities" ) ( "sch" ->> "countries" ) '[ '( "country_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "comp_addr" ) where
  type TRelDef Sch ( "sch" ->> "comp_addr" ) = 
    'RelDef ( "sch" ->> "companies" ) ( "sch" ->> "addresses" ) '[ '( "address_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "cust_addr" ) where
  type TRelDef Sch ( "sch" ->> "cust_addr" ) = 
    'RelDef ( "sch" ->> "customers" ) ( "sch" ->> "addresses" ) '[ '( "address_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "opos_article" ) where
  type TRelDef Sch ( "sch" ->> "opos_article" ) = 
    'RelDef ( "sch" ->> "order_positions" ) ( "sch" ->> "articles" ) '[ '( "article_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "opos_order" ) where
  type TRelDef Sch ( "sch" ->> "opos_order" ) = 
    'RelDef ( "sch" ->> "order_positions" ) ( "sch" ->> "orders" ) '[ '( "order_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "ord_cust" ) where
  type TRelDef Sch ( "sch" ->> "ord_cust" ) = 
    'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "customers" ) '[ '( "customer_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "ord_seller" ) where
  type TRelDef Sch ( "sch" ->> "ord_seller" ) = 
    'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "companies" ) '[ '( "seller_id"
      ,"id" ) ]

instance CRelDef Sch ( "sch" ->> "ord_trader" ) where
  type TRelDef Sch ( "sch" ->> "ord_trader" ) = 
    'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "companies" ) '[ '( "trader_id"
      ,"id" ) ]

instance CTabRels Sch ( "sch" ->> "addresses" ) where
  type TFrom Sch ( "sch" ->> "addresses" ) = 
    '[ ( "sch" ->> "address_city" ) ]

  type TTo Sch ( "sch" ->> "addresses" ) = 
    '[ ( "sch" ->> "comp_addr" ),( "sch" ->> "cust_addr" ) ]

instance CTabRels Sch ( "sch" ->> "articles" ) where
  type TFrom Sch ( "sch" ->> "articles" ) = 
    '[  ]

  type TTo Sch ( "sch" ->> "articles" ) = 
    '[ ( "sch" ->> "opos_article" ) ]

instance CTabRels Sch ( "sch" ->> "cities" ) where
  type TFrom Sch ( "sch" ->> "cities" ) = 
    '[ ( "sch" ->> "city_country" ) ]

  type TTo Sch ( "sch" ->> "cities" ) = 
    '[ ( "sch" ->> "address_city" ) ]

instance CTabRels Sch ( "sch" ->> "companies" ) where
  type TFrom Sch ( "sch" ->> "companies" ) = 
    '[ ( "sch" ->> "comp_addr" ) ]

  type TTo Sch ( "sch" ->> "companies" ) = 
    '[ ( "sch" ->> "ord_seller" ),( "sch" ->> "ord_trader" ) ]

instance CTabRels Sch ( "sch" ->> "countries" ) where
  type TFrom Sch ( "sch" ->> "countries" ) = 
    '[  ]

  type TTo Sch ( "sch" ->> "countries" ) = 
    '[ ( "sch" ->> "city_country" ) ]

instance CTabRels Sch ( "sch" ->> "customers" ) where
  type TFrom Sch ( "sch" ->> "customers" ) = 
    '[ ( "sch" ->> "cust_addr" ) ]

  type TTo Sch ( "sch" ->> "customers" ) = 
    '[ ( "sch" ->> "ord_cust" ) ]

instance CTabRels Sch ( "sch" ->> "order_positions" ) where
  type TFrom Sch ( "sch" ->> "order_positions" ) = 
    '[ ( "sch" ->> "opos_article" ),( "sch" ->> "opos_order" ) ]

  type TTo Sch ( "sch" ->> "order_positions" ) = 
    '[  ]

instance CTabRels Sch ( "sch" ->> "orders" ) where
  type TFrom Sch ( "sch" ->> "orders" ) = 
    '[ ( "sch" ->> "ord_cust" )
      ,( "sch" ->> "ord_seller" ),( "sch" ->> "ord_trader" ) ]

  type TTo Sch ( "sch" ->> "orders" ) = 
    '[ ( "sch" ->> "opos_order" ) ]

instance CSchema Sch where
  type TTabs Sch = '[ ( "sch" ->> "addresses" ),( "sch" ->> "articles" )
    ,( "sch" ->> "cities" ),( "sch" ->> "companies" )
    ,( "sch" ->> "countries" ),( "sch" ->> "customers" )
    ,( "sch" ->> "order_positions" ),( "sch" ->> "orders" ) ]

  type TTypes Sch = '[ ( "pg_catalog" ->> "_int4" ),( "pg_catalog" ->> "_text" )
    ,( "pg_catalog" ->> "date" ),( "pg_catalog" ->> "float8" )
    ,( "pg_catalog" ->> "int4" ),( "pg_catalog" ->> "int8" )
    ,( "pg_catalog" ->> "numeric" ),( "pg_catalog" ->> "text" )
    ,( "pg_catalog" ->> "timestamptz" ),( "sch" ->> "order_state" ) ]

