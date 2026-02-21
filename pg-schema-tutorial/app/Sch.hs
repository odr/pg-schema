{- HLINT ignore -}
{-# LANGUAGE UndecidableInstances #-}
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

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "app" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "app" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "city_id" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "city_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "home" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "home" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "numbers" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "numbers" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_int4" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "phones" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "phones" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "street" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "street" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "zipcode" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "zipcode" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "articles" ) "code" where
  type TFieldInfo Sch ( "sch" ->> "articles" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "articles" ) "created_at" where
  type TFieldInfo Sch ( "sch" ->> "articles" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "articles" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "articles" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "articles" ) "name" where
  type TFieldInfo Sch ( "sch" ->> "articles" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "articles" ) "updated_at" where
  type TFieldInfo Sch ( "sch" ->> "articles" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "cities" ) "country_id" where
  type TFieldInfo Sch ( "sch" ->> "cities" ) "country_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "cities" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "cities" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "cities" ) "name" where
  type TFieldInfo Sch ( "sch" ->> "cities" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "companies" ) "address_id" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "address_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "companies" ) "created_at" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "companies" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "companies" ) "name" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "companies" ) "updated_at" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "countries" ) "code" where
  type TFieldInfo Sch ( "sch" ->> "countries" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "countries" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "countries" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "countries" ) "name" where
  type TFieldInfo Sch ( "sch" ->> "countries" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "customers" ) "address_id" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "address_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "customers" ) "created_at" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "customers" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "customers" ) "name" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "customers" ) "note" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "note" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "customers" ) "updated_at" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "article_id" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "article_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "cnt" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "cnt" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "num" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "num" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "order_id" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "order_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "price" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "price" = 'RFPlain ('FldDef ( "pg_catalog" ->> "numeric" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "created_at" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "customer_id" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "customer_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "day" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "day" = 'RFPlain ('FldDef ( "pg_catalog" ->> "date" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "id" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "num" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "num" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "seller_id" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "seller_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "state" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "state" = 'RFPlain ('FldDef ( "sch" ->> "order_state" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "trader_id" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "trader_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "orders" ) "updated_at" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "comp_addr" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "comp_addr" = 
    'RFToHere ( "sch" ->> "companies" ) '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "cust_addr" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "cust_addr" = 
    'RFToHere ( "sch" ->> "customers" ) '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "articles" ) "opos_article" where
  type TFieldInfo Sch ( "sch" ->> "articles" ) "opos_article" = 
    'RFToHere ( "sch" ->> "order_positions" ) '[ 'Ref "article_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "cities" ) "address_city" where
  type TFieldInfo Sch ( "sch" ->> "cities" ) "address_city" = 
    'RFToHere ( "sch" ->> "addresses" ) '[ 'Ref "city_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "companies" ) "ord_seller" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "ord_seller" = 
    'RFToHere ( "sch" ->> "orders" ) '[ 'Ref "seller_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "companies" ) "ord_trader" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "ord_trader" = 
    'RFToHere ( "sch" ->> "orders" ) '[ 'Ref "trader_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "countries" ) "city_country" where
  type TFieldInfo Sch ( "sch" ->> "countries" ) "city_country" = 
    'RFToHere ( "sch" ->> "cities" ) '[ 'Ref "country_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "customers" ) "ord_cust" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "ord_cust" = 
    'RFToHere ( "sch" ->> "orders" ) '[ 'Ref "customer_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "orders" ) "opos_order" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "opos_order" = 
    'RFToHere ( "sch" ->> "order_positions" ) '[ 'Ref "order_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "addresses" ) "address_city" where
  type TFieldInfo Sch ( "sch" ->> "addresses" ) "address_city" = 
    'RFFromHere ( "sch" ->> "cities" ) '[ 'Ref "city_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "cities" ) "city_country" where
  type TFieldInfo Sch ( "sch" ->> "cities" ) "city_country" = 
    'RFFromHere ( "sch" ->> "countries" ) '[ 'Ref "country_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "companies" ) "comp_addr" where
  type TFieldInfo Sch ( "sch" ->> "companies" ) "comp_addr" = 
    'RFFromHere ( "sch" ->> "addresses" ) '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "customers" ) "cust_addr" where
  type TFieldInfo Sch ( "sch" ->> "customers" ) "cust_addr" = 
    'RFFromHere ( "sch" ->> "addresses" ) '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "opos_article" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "opos_article" = 
    'RFFromHere ( "sch" ->> "articles" ) '[ 'Ref "article_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "order_positions" ) "opos_order" where
  type TFieldInfo Sch ( "sch" ->> "order_positions" ) "opos_order" = 
    'RFFromHere ( "sch" ->> "orders" ) '[ 'Ref "order_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "orders" ) "ord_cust" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "ord_cust" = 
    'RFFromHere ( "sch" ->> "customers" ) '[ 'Ref "customer_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "orders" ) "ord_seller" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "ord_seller" = 
    'RFFromHere ( "sch" ->> "companies" ) '[ 'Ref "seller_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

instance CFieldInfo Sch ( "sch" ->> "orders" ) "ord_trader" where
  type TFieldInfo Sch ( "sch" ->> "orders" ) "ord_trader" = 
    'RFFromHere ( "sch" ->> "companies" ) '[ 'Ref "trader_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]

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

