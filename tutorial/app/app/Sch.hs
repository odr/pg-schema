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

data instance PGEnum Sch ( "tut" ->> "project_status" )
  = Project_status_draft | Project_status_active | Project_status_archived
  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)

instance Hashable (PGEnum Sch ( "tut" ->> "project_status" ))

instance NFData (PGEnum Sch ( "tut" ->> "project_status" ))

type family TTypDefSch (name :: NameNSK) :: TypDef' TL.Symbol where
  TTypDefSch ( "pg_catalog" ->> "_int8" ) = 'TypDef "A" ('Just ( "pg_catalog" ->> "int8" )) '[  ]
  TTypDefSch ( "pg_catalog" ->> "_text" ) = 'TypDef "A" ('Just ( "pg_catalog" ->> "text" )) '[  ]
  TTypDefSch ( "pg_catalog" ->> "float8" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "int4" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "int8" ) = 'TypDef "N" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "jsonb" ) = 'TypDef "U" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "text" ) = 'TypDef "S" 'Nothing '[  ]
  TTypDefSch ( "pg_catalog" ->> "timestamptz" ) = 'TypDef "D" 'Nothing '[  ]
  TTypDefSch ( "tut" ->> "project_status" ) = 'TypDef "E" 'Nothing '[ "draft","active","archived" ]
  TTypDefSch name = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "type " TE.:<>: TE.ShowType name TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Types: pg_catalog._int8, pg_catalog._text, pg_catalog.float8, pg_catalog.int4, pg_catalog.int8, pg_catalog.jsonb, pg_catalog.text, pg_catalog.timestamptz, tut.project_status."
    TE.:$$: TE.Text "")
instance (ToStar (TTypDef Sch name), ToStar name) => CTypDef Sch name where
  type TTypDef Sch name = TTypDefSch name

type family TTabDefSch (name :: NameNSK) :: TabDef' TL.Symbol where
  TTabDefSch ( "tut" ->> "projects" ) = 'TabDef '[ "id","owner_id","title","status","tags","created_at" ] '[ "id" ] '[  ]
  TTabDefSch ( "tut" ->> "task_events" ) = 'TabDef '[ "project_id","seq","event_no","kind","meta","created_at" ] '[ "project_id","seq","event_no" ] '[  ]
  TTabDefSch ( "tut" ->> "tasks" ) = 'TabDef '[ "project_id","seq","title","priority","payload","watchers" ] '[ "project_id","seq" ] '[  ]
  TTabDefSch ( "tut" ->> "users" ) = 'TabDef '[ "id","name","email","created_at" ] '[ "id" ] '[ '[ "email" ] ]
  TTabDefSch name = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "table " TE.:<>: TE.ShowType name TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: tut.projects, tut.task_events, tut.tasks, tut.users."
    TE.:$$: TE.Text "")
instance (ToStar (TTabDef Sch name), ToStar name) => CTabDef Sch name where
  type TTabDef Sch name = TTabDefSch name

type family TRelDefSch (ref :: NameNSK) :: RelDef' TL.Symbol where
  TRelDefSch ( "tut" ->> "projects_owner_id_fkey" ) = 'RelDef ( "tut" ->> "projects" ) ( "tut" ->> "users" ) '[ '( "owner_id","id" ) ]
  TRelDefSch ( "tut" ->> "task_events_project_id_seq_fkey" ) = 'RelDef ( "tut" ->> "task_events" ) ( "tut" ->> "tasks" ) '[ '( "project_id","project_id" ),'( "seq","seq" ) ]
  TRelDefSch ( "tut" ->> "tasks_project_id_fkey" ) = 'RelDef ( "tut" ->> "tasks" ) ( "tut" ->> "projects" ) '[ '( "project_id","id" ) ]
  TRelDefSch ref = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "relation " TE.:<>: TE.ShowType ref TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Relations: tut.projects_owner_id_fkey, tut.task_events_project_id_seq_fkey, tut.tasks_project_id_fkey."
    TE.:$$: TE.Text "")
instance ( ToStar (TRelDef Sch ref)
         , CTabDef Sch (RdFrom (TRelDef Sch ref))
         , CTabDef Sch (RdTo (TRelDef Sch ref)) )
  => CRelDef Sch ref where
  type TRelDef Sch ref = TRelDefSch ref

type family TFromSch (tab :: NameNSK) :: [NameNSK] where
  TFromSch ( "tut" ->> "projects" ) = '[ ( "tut" ->> "projects_owner_id_fkey" ) ]

  TFromSch ( "tut" ->> "task_events" ) = '[ ( "tut" ->> "task_events_project_id_seq_fkey" ) ]

  TFromSch ( "tut" ->> "tasks" ) = '[ ( "tut" ->> "tasks_project_id_fkey" ) ]

  TFromSch ( "tut" ->> "users" ) = '[  ]

  TFromSch tab = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "TFrom for table " TE.:<>: TE.ShowType tab TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: tut.projects, tut.task_events, tut.tasks, tut.users."
    TE.:$$: TE.Text "")

type family TToSch (tab :: NameNSK) :: [NameNSK] where
  TToSch ( "tut" ->> "projects" ) = '[ ( "tut" ->> "tasks_project_id_fkey" ) ]

  TToSch ( "tut" ->> "task_events" ) = '[  ]

  TToSch ( "tut" ->> "tasks" ) = '[ ( "tut" ->> "task_events_project_id_seq_fkey" ) ]

  TToSch ( "tut" ->> "users" ) = '[ ( "tut" ->> "projects_owner_id_fkey" ) ]

  TToSch tab = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "TTo for table " TE.:<>: TE.ShowType tab TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: tut.projects, tut.task_events, tut.tasks, tut.users."
    TE.:$$: TE.Text "")
instance CTabRels Sch tab where
  type TFrom Sch tab = TFromSch tab
  type TTo Sch tab = TToSch tab

type family TDBFieldInfoSch (t :: NameNSK) (f :: TL.Symbol) :: RecFieldK NameNSK where
  TDBFieldInfoSch ( "tut" ->> "projects" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "projects" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "projects" ) "owner_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "projects" ) "status" = 'RFPlain ('FldDef ( "tut" ->> "project_status" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "projects" ) "tags" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_text" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "projects" ) "title" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "projects" ) "tasks_project_id_fkey" = 'RFToHere ( "tut" ->> "tasks" )
      '[ 'Ref "project_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'True) ]
  TDBFieldInfoSch ( "tut" ->> "projects" ) "projects_owner_id_fkey" = 'RFFromHere ( "tut" ->> "users" )
      '[ 'Ref "owner_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'True) ]
  TDBFieldInfoSch ( "tut" ->> "projects" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "tut" ->> "projects" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, owner_id, title, status, tags, created_at."
    TE.:$$: TE.Text "  Foreign key constraints: projects_owner_id_fkey, tasks_project_id_fkey."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "event_no" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "kind" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "meta" = 'RFPlain ('FldDef ( "pg_catalog" ->> "jsonb" ) 'True 'False)
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "project_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "seq" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "task_events" ) "task_events_project_id_seq_fkey" = 'RFFromHere ( "tut" ->> "tasks" )
      '[ 'Ref "project_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False) "project_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False)
      , 'Ref "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) ]
  TDBFieldInfoSch ( "tut" ->> "task_events" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "tut" ->> "task_events" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: project_id, seq, event_no, kind, meta, created_at."
    TE.:$$: TE.Text "  Foreign key constraints: task_events_project_id_seq_fkey."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "payload" = 'RFPlain ('FldDef ( "pg_catalog" ->> "jsonb" ) 'True 'False)
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "priority" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "project_id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "seq" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "title" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "watchers" = 'RFPlain ('FldDef ( "pg_catalog" ->> "_int8" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "task_events_project_id_seq_fkey" = 'RFToHere ( "tut" ->> "task_events" )
      '[ 'Ref "project_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False) "project_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False)
      , 'Ref "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) "seq" ('FldDef ( "pg_catalog" ->> "int4" ) 'False 'False) ]
  TDBFieldInfoSch ( "tut" ->> "tasks" ) "tasks_project_id_fkey" = 'RFFromHere ( "tut" ->> "projects" )
      '[ 'Ref "project_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'True) ]
  TDBFieldInfoSch ( "tut" ->> "tasks" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "tut" ->> "tasks" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: project_id, seq, title, priority, payload, watchers."
    TE.:$$: TE.Text "  Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch ( "tut" ->> "users" ) "created_at" = 'RFPlain ('FldDef ( "pg_catalog" ->> "timestamptz" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "users" ) "email" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'True 'False)
  TDBFieldInfoSch ( "tut" ->> "users" ) "id" = 'RFPlain ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'True)
  TDBFieldInfoSch ( "tut" ->> "users" ) "name" = 'RFPlain ('FldDef ( "pg_catalog" ->> "text" ) 'False 'False)
  TDBFieldInfoSch ( "tut" ->> "users" ) "projects_owner_id_fkey" = 'RFToHere ( "tut" ->> "projects" )
      '[ 'Ref "owner_id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'False) "id" ('FldDef ( "pg_catalog" ->> "int8" ) 'False 'True) ]
  TDBFieldInfoSch ( "tut" ->> "users" ) f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch
    TE.:$$: TE.Text "for table " TE.:<>: TE.ShowType ( "tut" ->> "users" )
    TE.:$$: TE.Text "name " TE.:<>: TE.ShowType f TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Fields: id, name, email, created_at."
    TE.:$$: TE.Text "  Foreign key constraints: projects_owner_id_fkey."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Your source or target type or renaimer is probably invalid."
    TE.:$$: TE.Text "")
  TDBFieldInfoSch t f = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:<>: TE.Text " the table " TE.:<>: TE.ShowType t TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text "")

instance (ToStar (TDBFieldInfo Sch t f), ToStar t, ToStar f) => CDBFieldInfo Sch t f where
  type TDBFieldInfo Sch t f = TDBFieldInfoSch t f

instance CSchema Sch where
  type TTabs Sch = '[ ( "tut" ->> "projects" ),( "tut" ->> "task_events" )
    ,( "tut" ->> "tasks" ),( "tut" ->> "users" ) ]

  type TTypes Sch = '[ ( "pg_catalog" ->> "_int8" )
    ,( "pg_catalog" ->> "_text" ),( "pg_catalog" ->> "float8" )
    ,( "pg_catalog" ->> "int4" ),( "pg_catalog" ->> "int8" )
    ,( "pg_catalog" ->> "jsonb" ),( "pg_catalog" ->> "text" )
    ,( "pg_catalog" ->> "timestamptz" ),( "tut" ->> "project_status" ) ]

