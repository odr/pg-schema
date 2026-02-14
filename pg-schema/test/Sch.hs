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

instance CTypDef Sch ( "pg_catalog" ->> "_bool" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_bool" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "bool" )) '[  ]

instance CTypDef Sch ( "pg_catalog" ->> "_bytea" ) where
  type TTypDef Sch ( "pg_catalog" ->> "_bytea" ) = 
    'TypDef "A" ('Just ( "pg_catalog" ->> "bytea" )) '[  ]

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

instance CTypDef Sch ( "test_schema" ->> "_color" ) where
  type TTypDef Sch ( "test_schema" ->> "_color" ) = 
    'TypDef "A" ('Just ( "test_schema" ->> "color" )) '[  ]

instance CTypDef Sch ( "test_schema" ->> "color" ) where
  type TTypDef Sch ( "test_schema" ->> "color" ) = 
    'TypDef "E" 'Nothing '[ "red","green","blue" ]

data instance PGEnum Sch ( "test_schema" ->> "color" )
  = Color_red | Color_green | Color_blue
  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)

instance Hashable (PGEnum Sch ( "test_schema" ->> "color" ))

instance NFData (PGEnum Sch ( "test_schema" ->> "color" ))

instance CFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "cboolean" where
  type TFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "cboolean" = 
    'FldDef ( "pg_catalog" ->> "_bool" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "cfloat8" where
  type TFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "cfloat8" = 
    'FldDef ( "pg_catalog" ->> "_float8" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "cint4" where
  type TFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "cint4" = 
    'FldDef ( "pg_catalog" ->> "_int4" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "ctext" where
  type TFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "ctext" = 
    'FldDef ( "pg_catalog" ->> "_text" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "ctimestamptz" where
  type TFldDef Sch ( "test_schema" ->> "base_arr_converts" ) "ctimestamptz" = 
    'FldDef ( "pg_catalog" ->> "_timestamptz" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "cboolean" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "cboolean" = 
    'FldDef ( "pg_catalog" ->> "bool" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "cdate" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "cdate" = 
    'FldDef ( "pg_catalog" ->> "date" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "cfloat8" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "cfloat8" = 
    'FldDef ( "pg_catalog" ->> "float8" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "cint4" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "cint4" = 
    'FldDef ( "pg_catalog" ->> "int4" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "ctext" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "ctext" = 
    'FldDef ( "pg_catalog" ->> "text" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "ctime" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "ctime" = 
    'FldDef ( "pg_catalog" ->> "time" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "ctimestamp" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "ctimestamp" = 
    'FldDef ( "pg_catalog" ->> "timestamp" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "base_converts" ) "ctimestamptz" where
  type TFldDef Sch ( "test_schema" ->> "base_converts" ) "ctimestamptz" = 
    'FldDef ( "pg_catalog" ->> "timestamptz" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "cbytea" where
  type TFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "cbytea" = 
    'FldDef ( "pg_catalog" ->> "_bytea" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "ccitext" where
  type TFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "ccitext" = 
    'FldDef ( "public" ->> "_citext" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "ccolor" where
  type TFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "ccolor" = 
    'FldDef ( "test_schema" ->> "_color" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "cjsonb" where
  type TFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "cjsonb" = 
    'FldDef ( "pg_catalog" ->> "_jsonb" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "cuuid" where
  type TFldDef Sch ( "test_schema" ->> "ext_arr_converts" ) "cuuid" = 
    'FldDef ( "pg_catalog" ->> "_uuid" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_converts" ) "cbytea" where
  type TFldDef Sch ( "test_schema" ->> "ext_converts" ) "cbytea" = 
    'FldDef ( "pg_catalog" ->> "bytea" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_converts" ) "ccitext" where
  type TFldDef Sch ( "test_schema" ->> "ext_converts" ) "ccitext" = 
    'FldDef ( "public" ->> "citext" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_converts" ) "ccolor" where
  type TFldDef Sch ( "test_schema" ->> "ext_converts" ) "ccolor" = 
    'FldDef ( "test_schema" ->> "color" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_converts" ) "cjson" where
  type TFldDef Sch ( "test_schema" ->> "ext_converts" ) "cjson" = 
    'FldDef ( "pg_catalog" ->> "json" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_converts" ) "cjsonb" where
  type TFldDef Sch ( "test_schema" ->> "ext_converts" ) "cjsonb" = 
    'FldDef ( "pg_catalog" ->> "jsonb" ) 'True 'False

instance CFldDef Sch ( "test_schema" ->> "ext_converts" ) "cuuid" where
  type TFldDef Sch ( "test_schema" ->> "ext_converts" ) "cuuid" = 
    'FldDef ( "pg_catalog" ->> "uuid" ) 'True 'False

instance CTabDef Sch ( "test_schema" ->> "base_arr_converts" ) where
  type TTabDef Sch ( "test_schema" ->> "base_arr_converts" ) = 
    'TabDef '[ "cboolean"
      ,"cint4","cfloat8","ctimestamptz","ctext" ] '[  ] '[  ]

instance CTabDef Sch ( "test_schema" ->> "base_converts" ) where
  type TTabDef Sch ( "test_schema" ->> "base_converts" ) = 
    'TabDef '[ "cboolean","cint4"
      ,"cfloat8","cdate","ctime","ctimestamp","ctimestamptz","ctext" ] '[  ] '[  ]

instance CTabDef Sch ( "test_schema" ->> "ext_arr_converts" ) where
  type TTabDef Sch ( "test_schema" ->> "ext_arr_converts" ) = 
    'TabDef '[ "ccitext","cbytea","cjsonb","cuuid","ccolor" ] '[  ] '[  ]

instance CTabDef Sch ( "test_schema" ->> "ext_converts" ) where
  type TTabDef Sch ( "test_schema" ->> "ext_converts" ) = 
    'TabDef '[ "ccitext"
      ,"cbytea","cjsonb","cjson","cuuid","ccolor" ] '[  ] '[  ]

instance CTabRels Sch ( "test_schema" ->> "base_arr_converts" ) where
  type TFrom Sch ( "test_schema" ->> "base_arr_converts" ) = 
    '[  ]

  type TTo Sch ( "test_schema" ->> "base_arr_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_schema" ->> "base_converts" ) where
  type TFrom Sch ( "test_schema" ->> "base_converts" ) = 
    '[  ]

  type TTo Sch ( "test_schema" ->> "base_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_schema" ->> "ext_arr_converts" ) where
  type TFrom Sch ( "test_schema" ->> "ext_arr_converts" ) = 
    '[  ]

  type TTo Sch ( "test_schema" ->> "ext_arr_converts" ) = 
    '[  ]

instance CTabRels Sch ( "test_schema" ->> "ext_converts" ) where
  type TFrom Sch ( "test_schema" ->> "ext_converts" ) = 
    '[  ]

  type TTo Sch ( "test_schema" ->> "ext_converts" ) = 
    '[  ]

instance CSchema Sch where
  type TTabs Sch = '[ ( "test_schema" ->> "base_arr_converts" )
    ,( "test_schema" ->> "base_converts" )
    ,( "test_schema" ->> "ext_arr_converts" )
    ,( "test_schema" ->> "ext_converts" ) ]

  type TTypes Sch = '[ ( "pg_catalog" ->> "_bool" )
    ,( "pg_catalog" ->> "_bytea" ),( "pg_catalog" ->> "_float8" )
    ,( "pg_catalog" ->> "_int4" ),( "pg_catalog" ->> "_jsonb" )
    ,( "pg_catalog" ->> "_text" ),( "pg_catalog" ->> "_timestamptz" )
    ,( "pg_catalog" ->> "_uuid" ),( "pg_catalog" ->> "bool" )
    ,( "pg_catalog" ->> "bytea" ),( "pg_catalog" ->> "date" )
    ,( "pg_catalog" ->> "float8" ),( "pg_catalog" ->> "int4" )
    ,( "pg_catalog" ->> "int8" ),( "pg_catalog" ->> "json" )
    ,( "pg_catalog" ->> "jsonb" ),( "pg_catalog" ->> "text" )
    ,( "pg_catalog" ->> "time" ),( "pg_catalog" ->> "timestamp" )
    ,( "pg_catalog" ->> "timestamptz" ),( "pg_catalog" ->> "uuid" )
    ,( "public" ->> "_citext" ),( "public" ->> "citext" )
    ,( "test_schema" ->> "_color" ),( "test_schema" ->> "color" ) ]

