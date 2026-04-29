```haskell
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

...

instance (ToStar (TDBFieldInfo Sch t f), ToStar t, ToStar f) => CDBFieldInfo Sch t f where
  type TDBFieldInfo Sch t f = TDBFieldInfoSch t f
```
