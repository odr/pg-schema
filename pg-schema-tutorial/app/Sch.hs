{- HLINT ignore -}
{-# LANGUAGE FlexibleContexts #-}
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

data instance PGEnum Sch ( "sch" ->> "order_state" )
  = Order_state_paid | Order_state_booked | Order_state_delivered
  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)

instance Hashable (PGEnum Sch ( "sch" ->> "order_state" ))

instance NFData (PGEnum Sch ( "sch" ->> "order_state" ))

type family TTypDefSch (name :: NameNSK) :: TypDef' TL.Symbol where
  TTypDefSch ( "pg_catalog" ->> "_int4" ) = 'TypDef "A" ('Just ( "pg_catalog" ->> "int4" )) '[  ]
  TTypDefSch ( "pg_catalog" ->> "_text" ) = 'TypDef "A" ('Just ( "pg_catalog" ->> "text" )) '[  ]
  TTypDefSch ( "pg_catalog" ->> "date" ) = 'TypDef "D" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "float8" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "int4" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "int8" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "numeric" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "text" ) = 'TypDef "S" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "timestamptz" ) = 'TypDef "D" 'Nothing '[  ]
  TTypDefSch ( "sch" ->> "order_state" ) = 'TypDef "E" 'Nothing '[ "paid","booked","delivered" ]
  TTypDefSch name = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "type " TE.:<>: TE.ShowType name TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Types: pg_catalog._int4, pg_catalog._text, pg_catalog.date, pg_catalog.float8, pg_catalog.int4, pg_catalog.int8, pg_catalog.numeric, pg_catalog.text, pg_catalog.timestamptz, sch.order_state."
    TE.:$$: TE.Text "")
instance (ToStar (TTypDef Sch name), ToStar name) => CTypDef Sch name where
  type TTypDef Sch name = TTypDefSch name

type family TTabDefSch (name :: NameNSK) :: TabDef' TL.Symbol where
  TTabDefSch ( "sch" ->> "addresses" ) = 'TabDef '[ "id","city_id","street","home","app","zipcode","phones","numbers" ] '[ "id" ] '[  ]
  TTabDefSch ( "sch" ->> "articles" ) = 'TabDef '[ "id","name","code","created_at","updated_at" ] '[ "id" ] '[ '[ "name" ] ]
  TTabDefSch ( "sch" ->> "cities" ) = 'TabDef '[ "id","country_id","name" ] '[ "id" ] '[  ]
  TTabDefSch ( "sch" ->> "companies" ) = 'TabDef '[ "id","name","address_id","created_at","updated_at" ] '[ "id" ] '[  ]
  TTabDefSch ( "sch" ->> "countries" ) = 'TabDef '[ "id","name","code" ] '[ "id" ] '[  ]
  TTabDefSch ( "sch" ->> "customers" ) = 'TabDef '[ "id","name","address_id","note","created_at","updated_at" ] '[ "id" ] '[  ]
  TTabDefSch ( "sch" ->> "order_positions" ) = 'TabDef '[ "order_id","num","article_id","cnt","price" ] '[ "order_id","num" ] '[ '[ "order_id","article_id" ] ]
  TTabDefSch ( "sch" ->> "orders" ) = 'TabDef '[ "id","day","num","customer_id","seller_id","trader_id","state","created_at","updated_at" ] '[ "id" ] '[  ]
  TTabDefSch name = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "table " TE.:<>: TE.ShowType name TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: sch.addresses, sch.articles, sch.cities, sch.companies, sch.countries, sch.customers, sch.order_positions, sch.orders."
    TE.:$$: TE.Text "")
instance (ToStar (TTabDef Sch name), ToStar name) => CTabDef Sch name where
  type TTabDef Sch name = TTabDefSch name

type family TRelDefSch (ref :: NameNSK) :: RelDef' TL.Symbol where
  TRelDefSch ( "sch" ->> "address_city" ) = 'RelDef ( "sch" ->> "addresses" ) ( "sch" ->> "cities" ) '[ '( "city_id","id" ) ]
  TRelDefSch ( "sch" ->> "city_country" ) = 'RelDef ( "sch" ->> "cities" ) ( "sch" ->> "countries" ) '[ '( "country_id","id" ) ]
  TRelDefSch ( "sch" ->> "comp_addr" ) = 'RelDef ( "sch" ->> "companies" ) ( "sch" ->> "addresses" ) '[ '( "address_id","id" ) ]
  TRelDefSch ( "sch" ->> "cust_addr" ) = 'RelDef ( "sch" ->> "customers" ) ( "sch" ->> "addresses" ) '[ '( "address_id","id" ) ]
  TRelDefSch ( "sch" ->> "opos_article" ) = 'RelDef ( "sch" ->> "order_positions" ) ( "sch" ->> "articles" ) '[ '( "article_id","id" ) ]
  TRelDefSch ( "sch" ->> "opos_order" ) = 'RelDef ( "sch" ->> "order_positions" ) ( "sch" ->> "orders" ) '[ '( "order_id","id" ) ]
  TRelDefSch ( "sch" ->> "ord_cust" ) = 'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "customers" ) '[ '( "customer_id","id" ) ]
  TRelDefSch ( "sch" ->> "ord_seller" ) = 'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "companies" ) '[ '( "seller_id","id" ) ]
  TRelDefSch ( "sch" ->> "ord_trader" ) = 'RelDef ( "sch" ->> "orders" ) ( "sch" ->> "companies" ) '[ '( "trader_id","id" ) ]
  TRelDefSch ref = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "relation " TE.:<>: TE.ShowType ref TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Relations: sch.address_city, sch.city_country, sch.comp_addr, sch.cust_addr, sch.opos_article, sch.opos_order, sch.ord_cust, sch.ord_seller, sch.ord_trader."
    TE.:$$: TE.Text "")
instance ( ToStar (TRelDef Sch ref)
         , CTabDef Sch (RdFrom (TRelDef Sch ref))
         , CTabDef Sch (RdTo (TRelDef Sch ref)) )
  => CRelDef Sch ref where
  type TRelDef Sch ref = TRelDefSch ref

type family TFromSch (tab :: NameNSK) :: [NameNSK] where
  TFromSch ( "sch" ->> "addresses" ) = '[ ( "sch" ->> "address_city" ) ]

  TFromSch ( "sch" ->> "articles" ) = '[  ]

  TFromSch ( "sch" ->> "cities" ) = '[ ( "sch" ->> "city_country" ) ]

  TFromSch ( "sch" ->> "companies" ) = '[ ( "sch" ->> "comp_addr" ) ]

  TFromSch ( "sch" ->> "countries" ) = '[  ]

  TFromSch ( "sch" ->> "customers" ) = '[ ( "sch" ->> "cust_addr" ) ]

  TFromSch ( "sch" ->> "order_positions" ) = '[ ( "sch" ->> "opos_article" ),( "sch" ->> "opos_order" ) ]

  TFromSch ( "sch" ->> "orders" ) = '[ ( "sch" ->> "ord_cust" )
    ,( "sch" ->> "ord_seller" ),( "sch" ->> "ord_trader" ) ]

  TFromSch tab = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "TFrom for table " TE.:<>: TE.ShowType tab TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: sch.addresses, sch.articles, sch.cities, sch.companies, sch.countries, sch.customers, sch.order_positions, sch.orders."
    TE.:$$: TE.Text "")

type family TToSch (tab :: NameNSK) :: [NameNSK] where
  TToSch ( "sch" ->> "addresses" ) = '[ ( "sch" ->> "comp_addr" ),( "sch" ->> "cust_addr" ) ]

  TToSch ( "sch" ->> "articles" ) = '[ ( "sch" ->> "opos_article" ) ]

  TToSch ( "sch" ->> "cities" ) = '[ ( "sch" ->> "address_city" ) ]

  TToSch ( "sch" ->> "companies" ) = '[ ( "sch" ->> "ord_seller" ),( "sch" ->> "ord_trader" ) ]

  TToSch ( "sch" ->> "countries" ) = '[ ( "sch" ->> "city_country" ) ]

  TToSch ( "sch" ->> "customers" ) = '[ ( "sch" ->> "ord_cust" ) ]

  TToSch ( "sch" ->> "order_positions" ) = '[  ]

  TToSch ( "sch" ->> "orders" ) = '[ ( "sch" ->> "opos_order" ) ]

  TToSch tab = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "TTo for table " TE.:<>: TE.ShowType tab TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: sch.addresses, sch.articles, sch.cities, sch.companies, sch.countries, sch.customers, sch.order_positions, sch.orders."
    TE.:$$: TE.Text "")
instance CTabRels Sch tab where
  type TFrom Sch tab = TFromSch tab
  type TTo Sch tab = TToSch tab

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

instance (ToStar (TDBFieldInfo Sch t f), ToStar t, ToStar f) => CDBFieldInfo Sch t f where
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

