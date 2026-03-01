{- HLINT ignore -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -freduction-depth=300 #-}
module Dml where

-- This file is generated and can't be edited.

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import GHC.TypeError qualified as TE
import GHC.TypeLits qualified as TL
import Database.Schema.Def
import Database.PostgreSQL.Enum


data Dml

instance CTypDef Dml ( "pg_catalog" ->> "_date" ) where
  type TTypDef Dml ( "pg_catalog" ->> "_date" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "date" )) '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "_jsonb" ) where
  type TTypDef Dml ( "pg_catalog" ->> "_jsonb" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "jsonb" )) '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "bool" ) where
  type TTypDef Dml ( "pg_catalog" ->> "bool" ) = 
    'TypDef "B" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "float8" ) where
  type TTypDef Dml ( "pg_catalog" ->> "float8" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "int4" ) where
  type TTypDef Dml ( "pg_catalog" ->> "int4" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "int8" ) where
  type TTypDef Dml ( "pg_catalog" ->> "int8" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "jsonb" ) where
  type TTypDef Dml ( "pg_catalog" ->> "jsonb" ) = 
    'TypDef "U" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "numeric" ) where
  type TTypDef Dml ( "pg_catalog" ->> "numeric" ) = 
    'TypDef "N" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "text" ) where
  type TTypDef Dml ( "pg_catalog" ->> "text" ) = 
    'TypDef "S" 'Nothing '[  ]

instance CTypDef Dml ( "pg_catalog" ->> "timestamptz" ) where
  type TTypDef Dml ( "pg_catalog" ->> "timestamptz" ) = 
    'TypDef "D" 'Nothing '[  ]

instance CTabDef Dml ( "test_dml" ->> "arrays" ) where
  type TTabDef Dml ( "test_dml" ->> "arrays" ) = 
    'TabDef '[ "id","root_id","dates_nullable","jsons" ] '[ "id" ] '[  ]

instance CTabDef Dml ( "test_dml" ->> "dim" ) where
  type TTabDef Dml ( "test_dml" ->> "dim" ) = 
    'TabDef '[ "id","code","name" ] '[ "id" ] '[ '[ "code" ] ]

instance CTabDef Dml ( "test_dml" ->> "leaf" ) where
  type TTabDef Dml ( "test_dml" ->> "leaf" ) = 
    'TabDef '[ "root_id","seq","leaf_no"
      ,"value","category","created_at" ] '[ "root_id","seq","leaf_no" ] '[  ]

instance CTabDef Dml ( "test_dml" ->> "mid1" ) where
  type TTabDef Dml ( "test_dml" ->> "mid1" ) = 
    'TabDef '[ "id"
      ,"root_id","pos","flag","sort_key","payload" ] '[ "id" ] '[  ]

instance CTabDef Dml ( "test_dml" ->> "mid2" ) where
  type TTabDef Dml ( "test_dml" ->> "mid2" ) = 
    'TabDef '[ "root_id"
      ,"seq","kind","priority","payload" ] '[ "root_id","seq" ] '[  ]

instance CTabDef Dml ( "test_dml" ->> "root" ) where
  type TTabDef Dml ( "test_dml" ->> "root" ) = 
    'TabDef '[ "id","code","grp","name"
      ,"created_at","dim_a_id","dim_b_id" ] '[ "id" ] '[ '[ "code","grp" ] ]

type instance TRelDef Dml
  ( "test_dml" ->> "arrays_root_fk" ) = 'RelDef ( "test_dml" ->> "arrays" ) ( "test_dml" ->> "root" ) '[ '( "root_id","id" ) ]

type instance TRelDef Dml
  ( "test_dml" ->> "leaf_mid2_fk" ) = 'RelDef ( "test_dml" ->> "leaf" ) ( "test_dml" ->> "mid2" ) '[ '( "root_id","root_id" ),'( "seq","seq" ) ]

type instance TRelDef Dml
  ( "test_dml" ->> "mid1_root_fk" ) = 'RelDef ( "test_dml" ->> "mid1" ) ( "test_dml" ->> "root" ) '[ '( "root_id","id" ) ]

type instance TRelDef Dml
  ( "test_dml" ->> "mid2_root_fk" ) = 'RelDef ( "test_dml" ->> "mid2" ) ( "test_dml" ->> "root" ) '[ '( "root_id","id" ) ]

type instance TRelDef Dml
  ( "test_dml" ->> "root_dim_a_fk" ) = 'RelDef ( "test_dml" ->> "root" ) ( "test_dml" ->> "dim" ) '[ '( "dim_a_id","id" ) ]

type instance TRelDef Dml
  ( "test_dml" ->> "root_dim_b_fk" ) = 'RelDef ( "test_dml" ->> "root" ) ( "test_dml" ->> "dim" ) '[ '( "dim_b_id","id" ) ]

instance CTabRels Dml ( "test_dml" ->> "arrays" ) where
  type TFrom Dml ( "test_dml" ->> "arrays" ) = 
    '[ ( "test_dml" ->> "arrays_root_fk" ) ]

  type TTo Dml ( "test_dml" ->> "arrays" ) = 
    '[  ]

instance CTabRels Dml ( "test_dml" ->> "dim" ) where
  type TFrom Dml ( "test_dml" ->> "dim" ) = 
    '[  ]

  type TTo Dml ( "test_dml" ->> "dim" ) = 
    '[ ( "test_dml" ->> "root_dim_a_fk" )
      ,( "test_dml" ->> "root_dim_b_fk" ) ]

instance CTabRels Dml ( "test_dml" ->> "leaf" ) where
  type TFrom Dml ( "test_dml" ->> "leaf" ) = 
    '[ ( "test_dml" ->> "leaf_mid2_fk" ) ]

  type TTo Dml ( "test_dml" ->> "leaf" ) = 
    '[  ]

instance CTabRels Dml ( "test_dml" ->> "mid1" ) where
  type TFrom Dml ( "test_dml" ->> "mid1" ) = 
    '[ ( "test_dml" ->> "mid1_root_fk" ) ]

  type TTo Dml ( "test_dml" ->> "mid1" ) = 
    '[  ]

instance CTabRels Dml ( "test_dml" ->> "mid2" ) where
  type TFrom Dml ( "test_dml" ->> "mid2" ) = 
    '[ ( "test_dml" ->> "mid2_root_fk" ) ]

  type TTo Dml ( "test_dml" ->> "mid2" ) = 
    '[ ( "test_dml" ->> "leaf_mid2_fk" ) ]

instance CTabRels Dml ( "test_dml" ->> "root" ) where
  type TFrom Dml ( "test_dml" ->> "root" ) = 
    '[ ( "test_dml" ->> "root_dim_a_fk" )
      ,( "test_dml" ->> "root_dim_b_fk" ) ]

  type TTo Dml ( "test_dml" ->> "root" ) = 
    '[ ( "test_dml" ->> "arrays_root_fk" )
      ,( "test_dml" ->> "mid1_root_fk" ),( "test_dml" ->> "mid2_root_fk" ) ]

type family TFieldInfoDml (t :: NameNSK) (f :: TL.Symbol) :: RecFieldK NameNSK where
  TFieldInfoDml ( "test_dml" ->> "arrays" ) "dates_nullable" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_date" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "arrays" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "arrays" ) "jsons" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_jsonb" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "arrays" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "arrays" ) "arrays_root_fk" = 'RFFromHere ( "test_dml" ->> "root" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "arrays" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_dml" ->> "arrays" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, root_id, dates_nullable, jsons."
    TE.:$$: TE.Text "  Foreign key constraints: arrays_root_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoDml ( "test_dml" ->> "dim" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "dim" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "dim" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "dim" ) "root_dim_a_fk" = 'RFToHere ( "test_dml" ->> "root" )
      '[ 'Ref "dim_a_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "dim" ) "root_dim_b_fk" = 'RFToHere ( "test_dml" ->> "root" )
      '[ 'Ref "dim_b_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "dim" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_dml" ->> "dim" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, code, name."
    TE.:$$: TE.Text "  Foreign key constraints: root_dim_a_fk, root_dim_b_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "category" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "leaf_no" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "seq" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "value" = 'RFPlain ('FldDef ( "pg_catalog" ->> "numeric" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "leaf" ) "leaf_mid2_fk" = 'RFFromHere ( "test_dml" ->> "mid2" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
      , 'Ref "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) ]
  TFieldInfoDml ( "test_dml" ->> "leaf" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_dml" ->> "leaf" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: root_id, seq, leaf_no, value, category, created_at."
    TE.:$$: TE.Text "  Foreign key constraints: leaf_mid2_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "flag" = 'RFPlain ('FldDef ( "pg_catalog" ->> "bool" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "payload" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "pos" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "sort_key" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid1" ) "mid1_root_fk" = 'RFFromHere ( "test_dml" ->> "root" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "mid1" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_dml" ->> "mid1" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, root_id, pos, flag, sort_key, payload."
    TE.:$$: TE.Text "  Foreign key constraints: mid1_root_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "kind" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "payload" = 'RFPlain ('FldDef ( "pg_catalog" ->> "jsonb" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "priority" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "root_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "seq" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "leaf_mid2_fk" = 'RFToHere ( "test_dml" ->> "leaf" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
      , 'Ref "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) ]
  TFieldInfoDml ( "test_dml" ->> "mid2" ) "mid2_root_fk" = 'RFFromHere ( "test_dml" ->> "root" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "mid2" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_dml" ->> "mid2" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: root_id, seq, kind, priority, payload."
    TE.:$$: TE.Text "  Foreign key constraints: mid2_root_fk, leaf_mid2_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoDml ( "test_dml" ->> "root" ) "code" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "root" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "root" ) "dim_a_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "root" ) "dim_b_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False)
  TFieldInfoDml ( "test_dml" ->> "root" ) "grp" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "root" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TFieldInfoDml ( "test_dml" ->> "root" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TFieldInfoDml ( "test_dml" ->> "root" ) "arrays_root_fk" = 'RFToHere ( "test_dml" ->> "arrays" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "root" ) "mid1_root_fk" = 'RFToHere ( "test_dml" ->> "mid1" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "root" ) "mid2_root_fk" = 'RFToHere ( "test_dml" ->> "mid2" )
      '[ 'Ref "root_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "root" ) "root_dim_a_fk" = 'RFFromHere ( "test_dml" ->> "dim" )
      '[ 'Ref "dim_a_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "root" ) "root_dim_b_fk" = 'RFFromHere ( "test_dml" ->> "dim" )
      '[ 'Ref "dim_b_id" ('FldDef ( "pg_catalog" ->> "int4" ) 'True 'False) "id" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True) ]
  TFieldInfoDml ( "test_dml" ->> "root" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "test_dml" ->> "root" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, code, grp, name, created_at, dim_a_id, dim_b_id."
    TE.:$$: TE.Text "  Foreign key constraints: root_dim_a_fk, root_dim_b_fk, arrays_root_fk, mid1_root_fk, mid2_root_fk."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TFieldInfoDml t f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Dml TE.:<>: TE.Text " the table " TE.:<>: TE.ShowType t TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text "")

instance CFieldInfo Dml t f where
  type TFieldInfo Dml t f = TFieldInfoDml t f

instance CSchema Dml where
  type TTabs Dml = '[ ( "test_dml" ->> "arrays" ),( "test_dml" ->> "dim" )
    ,( "test_dml" ->> "leaf" ),( "test_dml" ->> "mid1" )
    ,( "test_dml" ->> "mid2" ),( "test_dml" ->> "root" ) ]

  type TTypes Dml = '[ ( "pg_catalog" ->> "_date" ),( "pg_catalog" ->> "_jsonb" )
    ,( "pg_catalog" ->> "bool" ),( "pg_catalog" ->> "float8" )
    ,( "pg_catalog" ->> "int4" ),( "pg_catalog" ->> "int8" )
    ,( "pg_catalog" ->> "jsonb" ),( "pg_catalog" ->> "numeric" )
    ,( "pg_catalog" ->> "text" ),( "pg_catalog" ->> "timestamptz" ) ]

