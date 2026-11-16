```haskell
type family TTabDefSch (name :: NameNSK) :: TabDef' TL.Symbol where
  TTabDefSch ( "audit" ->> "project_log" ) = 'TabDef '[ "id","client_ip","created_at","message","project_id" ] '[ "id" ] '[  ]
  TTabDefSch ( "tut" ->> "projects" ) = 'TabDef '[ "id","created_at","owner_id","status","tags","title" ] '[ "id" ] '[  ]
  TTabDefSch ( "tut" ->> "task_events" ) = 'TabDef '[ "project_id","seq","event_no","author_id","created_at","kind","meta" ] '[ "project_id","seq","event_no" ] '[  ]
  TTabDefSch ( "tut" ->> "tasks" ) = 'TabDef '[ "project_id","seq","payload","priority","title","watchers" ] '[ "project_id","seq" ] '[  ]
  TTabDefSch ( "tut" ->> "users" ) = 'TabDef '[ "id","created_at","email","manager_id","name" ] '[ "id" ] '[ '[ "email" ] ]
  TTabDefSch name = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "table " TE.:<>: TE.ShowType name TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: audit.project_log, tut.projects, tut.task_events, tut.tasks, tut.users."
    TE.:$$: TE.Text "")
instance (ToStar (TTabDef Sch name), ToStar name) => CTabDef Sch name where
  type TTabDef Sch name = TTabDefSch name
```
