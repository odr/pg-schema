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
import Database.Schema.Def
import Database.PostgreSQL.Enum


data Sch

instance CTypDef Sch ( "pg_catalog" ->> "_bool" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_bool" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "bool" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_bytea" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_bytea" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "bytea" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_date" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_date" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "date" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_float8" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_float8" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "float8" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_int4" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_int4" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "int4" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_jsonb" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_jsonb" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "jsonb" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_text" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_text" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "text" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_timestamptz" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_timestamptz" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "timestamptz" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_uuid" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_uuid" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "uuid" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "bool" ) where
  type TTypDef Sch ( "pg_catalog" ->> "bool" ) = 
    'TypDef "B" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "bytea" ) where
  type TTypDef Sch ( "pg_catalog" ->> "bytea" ) = 
    'TypDef "U" 'Nothing '[  ]

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

instance CTypDef Sch ( "pg_catalog" ->> "json" ) where
  type TTypDef Sch ( "pg_catalog" ->> "json" ) = 
    'TypDef "U" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "jsonb" ) where
  type TTypDef Sch ( "pg_catalog" ->> "jsonb" ) = 
    'TypDef "U" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "numeric" ) where
  type TTypDef Sch ( "pg_catalog" ->> "numeric" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "text" ) where
  type TTypDef Sch ( "pg_catalog" ->> "text" ) = 
    'TypDef "S" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "time" ) where
  type TTypDef Sch ( "pg_catalog" ->> "time" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "timestamp" ) where
  type TTypDef Sch ( "pg_catalog" ->> "timestamp" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "timestamptz" ) where
  type TTypDef Sch ( "pg_catalog" ->> "timestamptz" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "uuid" ) where
  type TTypDef Sch ( "pg_catalog" ->> "uuid" ) = 
    'TypDef "U" 'Nothing '[  ]

instance CTypDef Sch ( "public" ->> "_citext" ) where
  type TTypDef Sch ( "public" ->> "_citext" ) = 
    'TypDef "A" ('Just ( "public" ->> "citext" )) '[  ]

instance CTypDef Sch ( "public" ->> "citext" ) where
  type TTypDef Sch ( "public" ->> "citext" ) = 
    'TypDef "S" 'Nothing '[  ]

instance CTypDef Sch ( "test_pgs" ->> "_color" ) where
  type TTypDef Sch ( "test_pgs" ->> "_color" ) = 
    'TypDef "A" ('Just ( "test_pgs" ->> "color" )) '[  ]

instance CTypDef Sch ( "test_pgs" ->> "color" ) where
  type TTypDef Sch ( "test_pgs" ->> "color" ) = 
    'TypDef "E" 'Nothing '[ "red","green","blue" ]

data instance PGEnum Sch ( "test_pgs" ->> "color" )
  = Color_red | Color_green | Color_blue
  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)

instance Hashable (PGEnum Sch ( "test_pgs" ->> "color" ))

instance NFData (PGEnum Sch ( "test_pgs" ->> "color" ))

instance CTabDef Sch ( "test_pgs" ->> "arrays" ) where
  type TTabDef Sch ( "test_pgs" ->> "arrays" ) = 
    'TabDef '[ "id","root_id","dates_nullable","jsons" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "base_arr_converts" ) where
  type TTabDef Sch ( "test_pgs" ->> "base_arr_converts" ) = 
    'TabDef '[ "cboolean"
      ,"cint4","cfloat8","ctimestamptz","ctext" ] '[  ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "base_converts" ) where
  type TTabDef Sch ( "test_pgs" ->> "base_converts" ) = 
    'TabDef '[ "cboolean","cint4"
      ,"cfloat8","cdate","ctime","ctimestamp","ctimestamptz","ctext" ] '[  ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "dim" ) where
  type TTabDef Sch ( "test_pgs" ->> "dim" ) = 
    'TabDef '[ "id","code","name" ] '[ "id" ] '[ '[ "code" ] ]

instance CTabDef Sch ( "test_pgs" ->> "ext_arr_converts" ) where
  type TTabDef Sch ( "test_pgs" ->> "ext_arr_converts" ) = 
    'TabDef '[ "ccitext","cbytea","cjsonb","cuuid","ccolor" ] '[  ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "ext_converts" ) where
  type TTabDef Sch ( "test_pgs" ->> "ext_converts" ) = 
    'TabDef '[ "ccitext"
      ,"cbytea","cjsonb","cjson","cuuid","ccolor" ] '[  ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "leaf" ) where
  type TTabDef Sch ( "test_pgs" ->> "leaf" ) = 
    'TabDef '[ "root_id","seq","leaf_no"
      ,"value","category","created_at" ] '[ "root_id","seq","leaf_no" ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "mid1" ) where
  type TTabDef Sch ( "test_pgs" ->> "mid1" ) = 
    'TabDef '[ "id"
      ,"root_id","pos","flag","sort_key","payload" ] '[ "id" ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "mid2" ) where
  type TTabDef Sch ( "test_pgs" ->> "mid2" ) = 
    'TabDef '[ "root_id"
      ,"seq","kind","priority","payload" ] '[ "root_id","seq" ] '[  ]

instance CTabDef Sch ( "test_pgs" ->> "root" ) where
  type TTabDef Sch ( "test_pgs" ->> "root" ) = 
    'TabDef '[ "id","code","grp","name"
      ,"created_at","dim_a_id","dim_b_id" ] '[ "id" ] '[ '[ "code","grp" ] ]

type instance TRelDef Sch
  ( "test_pgs" ->> "arrays_root_fk" ) = 'RelDef ( "test_pgs" ->> "arrays" ) ( "test_pgs" ->> "root" ) '[ '( "root_id","id" ) ]

type instance TRelDef Sch
  ( "test_pgs" ->> "leaf_mid2_fk" ) = 'RelDef ( "test_pgs" ->> "leaf" ) ( "test_pgs" ->> "mid2" ) '[ '( "root_id","root_id" ),'( "seq","seq" ) ]

type instance TRelDef Sch
  ( "test_pgs" ->> "mid1_root_fk" ) = 'RelDef ( "test_pgs" ->> "mid1" ) ( "test_pgs" ->> "root" ) '[ '( "root_id","id" ) ]

type instance TRelDef Sch
  ( "test_pgs" ->> "mid2_root_fk" ) = 'RelDef ( "test_pgs" ->> "mid2" ) ( "test_pgs" ->> "root" ) '[ '( "root_id","id" ) ]

type instance TRelDef Sch
  ( "test_pgs" ->> "root_dim_a_fk" ) = 'RelDef ( "test_pgs" ->> "root" ) ( "test_pgs" ->> "dim" ) '[ '( "dim_a_id","id" ) ]

type instance TRelDef Sch
  ( "test_pgs" ->> "root_dim_b_fk" ) = 'RelDef ( "test_pgs" ->> "root" ) ( "test_pgs" ->> "dim" ) '[ '( "dim_b_id","id" ) ]

instance CTabRels Sch ( "test_pgs" ->> "arrays" ) where
  type TFrom Sch ( "test_pgs" ->> "arrays" ) = 
    '[ ( "test_pgs" ->> "arrays_root_fk" ) ]

  type TTo Sch ( "test_pgs" ->> "arrays" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "base_arr_converts" ) where
  type TFrom Sch ( "test_pgs" ->> "base_arr_converts" ) = 
    '[  ]

  type TTo Sch ( "test_pgs" ->> "base_arr_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "base_converts" ) where
  type TFrom Sch ( "test_pgs" ->> "base_converts" ) = 
    '[  ]

  type TTo Sch ( "test_pgs" ->> "base_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "dim" ) where
  type TFrom Sch ( "test_pgs" ->> "dim" ) = 
    '[  ]

  type TTo Sch ( "test_pgs" ->> "dim" ) = 
    '[ ( "test_pgs" ->> "root_dim_a_fk" )
      ,( "test_pgs" ->> "root_dim_b_fk" ) ]

instance CTabRels Sch ( "test_pgs" ->> "ext_arr_converts" ) where
  type TFrom Sch ( "test_pgs" ->> "ext_arr_converts" ) = 
    '[  ]

  type TTo Sch ( "test_pgs" ->> "ext_arr_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "ext_converts" ) where
  type TFrom Sch ( "test_pgs" ->> "ext_converts" ) = 
    '[  ]

  type TTo Sch ( "test_pgs" ->> "ext_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "leaf" ) where
  type TFrom Sch ( "test_pgs" ->> "leaf" ) = 
    '[ ( "test_pgs" ->> "leaf_mid2_fk" ) ]

  type TTo Sch ( "test_pgs" ->> "leaf" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "mid1" ) where
  type TFrom Sch ( "test_pgs" ->> "mid1" ) = 
    '[ ( "test_pgs" ->> "mid1_root_fk" ) ]

  type TTo Sch ( "test_pgs" ->> "mid1" ) = 
    '[  ]

instance CTabRels Sch ( "test_pgs" ->> "mid2" ) where
  type TFrom Sch ( "test_pgs" ->> "mid2" ) = 
    '[ ( "test_pgs" ->> "mid2_root_fk" ) ]

  type TTo Sch ( "test_pgs" ->> "mid2" ) = 
    '[ ( "test_pgs" ->> "leaf_mid2_fk" ) ]

instance CTabRels Sch ( "test_pgs" ->> "root" ) where
  type TFrom Sch ( "test_pgs" ->> "root" ) = 
    '[ ( "test_pgs" ->> "root_dim_a_fk" )
      ,( "test_pgs" ->> "root_dim_b_fk" ) ]

  type TTo Sch ( "test_pgs" ->> "root" ) = 
    '[ ( "test_pgs" ->> "arrays_root_fk" )
      ,( "test_pgs" ->> "mid1_root_fk" ),( "test_pgs" ->> "mid2_root_fk" ) ]

type family TFieldInfoSch (t :: NameNSK) (f :: TL.Symbol) :: RecFieldK NameNSK where
  TFieldInfoSch ( "test_pgs" ->> "arrays" ) "dates_nullable" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_date" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "arrays" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "arrays" ) "jsons" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_jsonb" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "arrays" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "arrays" ) "arrays_root_fk" = 'RFFromHere ( "test_pgs" ->> "root" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "arrays" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "arrays" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, root_id, dates_nullable, jsons."
    TE.:$$: TE.Text "  Foreign key constraints: arrays_root_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "base_arr_converts" ) "cboolean" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_bool" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_arr_converts" ) "cfloat8" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_float8" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_arr_converts" ) "cint4" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_int4" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_arr_converts" ) "ctext" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_text" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_arr_converts" ) "ctimestamptz" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_timestamptz" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_arr_converts" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "base_arr_converts" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: cboolean, cint4, cfloat8, ctimestamptz, ctext."
    TE.:$$: TE.Text "  Foreign key constraints: ."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "cboolean" = 'RFPlain ('FldDef ( "pg_catalog" ->> "bool" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "cdate" = 'RFPlain ('FldDef ( "pg_catalog" ->> "date" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "cfloat8" = 'RFPlain ('FldDef ( "pg_catalog" ->> "float8" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "cint4" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "ctext" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "ctime" = 'RFPlain ('FldDef ( "pg_catalog" ->> "time" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "ctimestamp" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamp" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) "ctimestamptz" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "base_converts" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "base_converts" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: cboolean, cint4, cfloat8, cdate, ctime, ctimestamp, ctimestamptz, ctext."
    TE.:$$: TE.Text "  Foreign key constraints: ."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "dim" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "dim" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "dim" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "dim" ) "root_dim_a_fk" = 'RFToHere ( "test_pgs" ->> "root" )
      '[ 'Ref "dim_a_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "dim" ) "root_dim_b_fk" = 'RFToHere ( "test_pgs" ->> "root" )
      '[ 'Ref "dim_b_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "dim" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "dim" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, code, name."
    TE.:$$: TE.Text "  Foreign key constraints: root_dim_a_fk, root_dim_b_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "ext_arr_converts" ) "cbytea" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_bytea" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_arr_converts" ) "ccitext" = 'RFPlain ('FldDef ( "public" ->> "_citext" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_arr_converts" ) "ccolor" = 'RFPlain ('FldDef ( "test_pgs" ->> "_color" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_arr_converts" ) "cjsonb" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_jsonb" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_arr_converts" ) "cuuid" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_uuid" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_arr_converts" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "ext_arr_converts" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: ccitext, cbytea, cjsonb, cuuid, ccolor."
    TE.:$$: TE.Text "  Foreign key constraints: ."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) "cbytea" = 'RFPlain ('FldDef ( "pg_catalog" ->> "bytea" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) "ccitext" = 'RFPlain ('FldDef ( "public" ->> "citext" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) "ccolor" = 'RFPlain ('FldDef ( "test_pgs" ->> "color" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) "cjson" = 'RFPlain ('FldDef ( "pg_catalog" ->> "json" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) "cjsonb" = 'RFPlain ('FldDef ( "pg_catalog" ->> "jsonb" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) "cuuid" = 'RFPlain ('FldDef ( "pg_catalog" ->> "uuid" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "ext_converts" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "ext_converts" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: ccitext, cbytea, cjsonb, cjson, cuuid, ccolor."
    TE.:$$: TE.Text "  Foreign key constraints: ."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "category" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "leaf_no" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "seq" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "value" = 'RFPlain ('FldDef ( "pg_catalog" ->> "numeric" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) "leaf_mid2_fk" = 'RFFromHere ( "test_pgs" ->> "mid2" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
      , 'Ref "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) ]
  TFieldInfoSch ( "test_pgs" ->> "leaf" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "leaf" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: root_id, seq, leaf_no, value, category, created_at."
    TE.:$$: TE.Text "  Foreign key constraints: leaf_mid2_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "flag" = 'RFPlain ('FldDef ( "pg_catalog" ->> "bool" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "payload" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "pos" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "sort_key" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) "mid1_root_fk" = 'RFFromHere ( "test_pgs" ->> "root" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "mid1" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "mid1" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, root_id, pos, flag, sort_key, payload."
    TE.:$$: TE.Text "  Foreign key constraints: mid1_root_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "kind" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "payload" = 'RFPlain ('FldDef ( "pg_catalog" ->> "jsonb" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "priority" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "seq" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "leaf_mid2_fk" = 'RFToHere ( "test_pgs" ->> "leaf" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
      , 'Ref "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) ]
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) "mid2_root_fk" = 'RFFromHere ( "test_pgs" ->> "root" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "mid2" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "mid2" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: root_id, seq, kind, priority, payload."
    TE.:$$: TE.Text "  Foreign key constraints: mid2_root_fk, leaf_mid2_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch ( "test_pgs" ->> "root" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "dim_a_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "dim_b_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "grp" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoSch ( "test_pgs" ->> "root" ) "arrays_root_fk" = 'RFToHere ( "test_pgs" ->> "arrays" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "root" ) "mid1_root_fk" = 'RFToHere ( "test_pgs" ->> "mid1" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "root" ) "mid2_root_fk" = 'RFToHere ( "test_pgs" ->> "mid2" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "root" ) "root_dim_a_fk" = 'RFFromHere ( "test_pgs" ->> "dim" )
      '[ 'Ref "dim_a_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "root" ) "root_dim_b_fk" = 'RFFromHere ( "test_pgs" ->> "dim" )
      '[ 'Ref "dim_b_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoSch ( "test_pgs" ->> "root" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_pgs" ->> "root" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, code, grp, name, created_at, dim_a_id, dim_b_id."
    TE.:$$: TE.Text "  Foreign key constraints: root_dim_a_fk, root_dim_b_fk, arrays_root_fk, mid1_root_fk, mid2_root_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoSch t f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:<>: TE.Text " the table " TE.:<>: TE.ShowType t TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text "")

instance CFieldInfo Sch t f where
  type TFieldInfo Sch t f = TFieldInfoSch t f

instance CSchema Sch where
  type TTabs Sch = '[ ( "test_pgs" ->> "arrays" ),( "test_pgs" ->> "base_arr_converts" )
    ,( "test_pgs" ->> "base_converts" ),( "test_pgs" ->> "dim" )
    ,( "test_pgs" ->> "ext_arr_converts" ),( "test_pgs" ->> "ext_converts" )
    ,( "test_pgs" ->> "leaf" ),( "test_pgs" ->> "mid1" )
    ,( "test_pgs" ->> "mid2" ),( "test_pgs" ->> "root" ) ]

  type TTypes Sch = '[ ( "pg_catalog" ->> "_bool" )
    ,( "pg_catalog" ->> "_bytea" ),( "pg_catalog" ->> "_date" )
    ,( "pg_catalog" ->> "_float8" ),( "pg_catalog" ->> "_int4" )
    ,( "pg_catalog" ->> "_jsonb" ),( "pg_catalog" ->> "_text" )
    ,( "pg_catalog" ->> "_timestamptz" ),( "pg_catalog" ->> "_uuid" )
    ,( "pg_catalog" ->> "bool" ),( "pg_catalog" ->> "bytea" )
    ,( "pg_catalog" ->> "date" ),( "pg_catalog" ->> "float8" )
    ,( "pg_catalog" ->> "int4" ),( "pg_catalog" ->> "int8" )
    ,( "pg_catalog" ->> "json" ),( "pg_catalog" ->> "jsonb" )
    ,( "pg_catalog" ->> "numeric" ),( "pg_catalog" ->> "text" )
    ,( "pg_catalog" ->> "time" ),( "pg_catalog" ->> "timestamp" )
    ,( "pg_catalog" ->> "timestamptz" ),( "pg_catalog" ->> "uuid" )
    ,( "public" ->> "_citext" ),( "public" ->> "citext" )
    ,( "test_pgs" ->> "_color" ),( "test_pgs" ->> "color" ) ]

