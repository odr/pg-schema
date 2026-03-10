{- HLINT ignore -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -freduction-depth=300 #-}
module Sch where

-- This file is generated and can't be edited.

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import GHC.TypeError qualified as TE
import GHC.TypeLits qualified as TL
import PgSchema.Import
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

type instance TRelDef Sch
  ( "sch" ->> "address_city" ) = 'RelDef ( "sch" ->> "addresses" ) ( "sch" ->> "cities" ) '[ '( "city_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "city_country" ) = 'RelDef ( "sch" ->> "cities" ) ( "sch" ->> "countries" ) '[ '( "country_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "comp_addr" ) = 'RelDef ( "sch" ->> "companies" ) ( "sch" ->> "addresses" ) '[ '( "address_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "cust_addr" ) = 'RelDef ( "sch" ->> "customers" ) ( "sch" ->> "addresses" ) '[ '( "address_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "opos_article" ) = 'RelDef ( "sch" ->> "order_positions" ) ( "sch" ->> "articles" ) '[ '( "article_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "opos_order" ) = 'RelDef ( "sch" ->> "order_positions" ) ( "sch" ->> "orders" ) '[ '( "order_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "ord_cust" ) = 'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "customers" ) '[ '( "customer_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "ord_seller" ) = 'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "companies" ) '[ '( "seller_id","id" ) ]

type instance TRelDef Sch
  ( "sch" ->> "ord_trader" ) = 'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "companies" ) '[ '( "trader_id","id" ) ]

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

type family TDBFieldInfoSch (t :: NameNSK) (f :: TL.Symbol) :: RecFieldK NameNSK where
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "app" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "city_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "home" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "numbers" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_int4" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "phones" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "street" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "zipcode" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "comp_addr" = 'RFToHere ( "sch" ->> "companies" )
      '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "cust_addr" = 'RFToHere ( "sch" ->> "customers" )
      '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "addresses" ) "address_city" = 'RFFromHere ( "sch" ->> "cities" )
      '[ 'Ref "city_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "addresses" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "addresses" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, city_id, street, home, app, zipcode, phones, numbers."
    TE.:$$: TE.Text "  Foreign key constraints: address_city, comp_addr, cust_addr."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "articles" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "articles" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "articles" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "articles" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "articles" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "articles" ) "opos_article" = 'RFToHere ( "sch" ->> "order_positions" )
      '[ 'Ref "article_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "articles" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "articles" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, name, code, created_at, updated_at."
    TE.:$$: TE.Text "  Foreign key constraints: opos_article."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "cities" ) "country_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "cities" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "cities" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "cities" ) "address_city" = 'RFToHere ( "sch" ->> "addresses" )
      '[ 'Ref "city_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "cities" ) "city_country" = 'RFFromHere ( "sch" ->> "countries" )
      '[ 'Ref "country_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "cities" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "cities" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, country_id, name."
    TE.:$$: TE.Text "  Foreign key constraints: city_country, address_city."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "companies" ) "address_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "companies" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "companies" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "companies" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "companies" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "companies" ) "ord_seller" = 'RFToHere ( "sch" ->> "orders" )
      '[ 'Ref "seller_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "companies" ) "ord_trader" = 'RFToHere ( "sch" ->> "orders" )
      '[ 'Ref "trader_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "companies" ) "comp_addr" = 'RFFromHere ( "sch" ->> "addresses" )
      '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "companies" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "companies" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, name, address_id, created_at, updated_at."
    TE.:$$: TE.Text "  Foreign key constraints: comp_addr, ord_seller, ord_trader."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "countries" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "countries" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "countries" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "countries" ) "city_country" = 'RFToHere ( "sch" ->> "cities" )
      '[ 'Ref "country_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "countries" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "countries" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, name, code."
    TE.:$$: TE.Text "  Foreign key constraints: city_country."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "customers" ) "address_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "customers" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "customers" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "customers" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "customers" ) "note" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "customers" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "customers" ) "ord_cust" = 'RFToHere ( "sch" ->> "orders" )
      '[ 'Ref "customer_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "customers" ) "cust_addr" = 'RFFromHere ( "sch" ->> "addresses" )
      '[ 'Ref "address_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "customers" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "customers" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, name, address_id, note, created_at, updated_at."
    TE.:$$: TE.Text "  Foreign key constraints: cust_addr, ord_cust."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "article_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "cnt" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "num" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "order_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "price" = 'RFPlain ('FldDef ( "pg_catalog" ->> "numeric" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "opos_article" = 'RFFromHere ( "sch" ->> "articles" )
      '[ 'Ref "article_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) "opos_order" = 'RFFromHere ( "sch" ->> "orders" )
      '[ 'Ref "order_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "order_positions" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "order_positions" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: order_id, num, article_id, cnt, price."
    TE.:$$: TE.Text "  Foreign key constraints: opos_article, opos_order."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "sch" ->> "orders" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "customer_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "day" = 'RFPlain ('FldDef ( "pg_catalog" ->> "date" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "num" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "seller_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "state" = 'RFPlain ('FldDef ( "sch" ->> "order_state" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "trader_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "updated_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)
  TDBFieldInfoSch ( "sch" ->> "orders" ) "opos_order" = 'RFToHere ( "sch" ->> "order_positions" )
      '[ 'Ref "order_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "orders" ) "ord_cust" = 'RFFromHere ( "sch" ->> "customers" )
      '[ 'Ref "customer_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "orders" ) "ord_seller" = 'RFFromHere ( "sch" ->> "companies" )
      '[ 'Ref "seller_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "orders" ) "ord_trader" = 'RFFromHere ( "sch" ->> "companies" )
      '[ 'Ref "trader_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TDBFieldInfoSch ( "sch" ->> "orders" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "sch" ->> "orders" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, day, num, customer_id, seller_id, trader_id, state, created_at, updated_at."
    TE.:$$: TE.Text "  Foreign key constraints: ord_cust, ord_seller, ord_trader, opos_order."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch t f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:<>: TE.Text " the table " TE.:<>: TE.ShowType t TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text "")

instance CDBFieldInfo Sch t f where
  type TDBFieldInfo Sch t f = TDBFieldInfoSch t f

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

