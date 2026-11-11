```haskell
type family TTabDefSch (name :: NameNSK) :: TabDef' TL.Symbol where
  TTabDefSch ( "audit" ->> "project_log" ) = 'TabDef '[ "id","project_id","message","created_at" ] '[ "id" ] '[  ]
  TTabDefSch ( "tut" ->> "projects" ) = 'TabDef '[ "id","owner_id","title","status","tags","created_at" ] '[ "id" ] '[  ]
  TTabDefSch ( "tut" ->> "task_events" ) = 'TabDef '[ "project_id","seq","event_no","kind","meta","author_id","created_at" ] '[ "project_id","seq","event_no" ] '[  ]
  TTabDefSch ( "tut" ->> "tasks" ) = 'TabDef '[ "project_id","seq","title","priority","payload","watchers" ] '[ "project_id","seq" ] '[  ]
  TTabDefSch ( "tut" ->> "users" ) = 'TabDef '[ "id","name","email","manager_id","created_at" ] '[ "id" ] '[ '[ "email" ] ]
  TTabDefSch name = TE.TypeError (TE.Text "In schema " TE.:<>: TE.ShowType Sch TE.:$$: TE.Text "table " TE.:<>: TE.ShowType name TE.:<>: TE.Text " is not defined."
    TE.:$$: TE.Text ""
    TE.:$$: TE.Text "Valid values are:"
    TE.:$$: TE.Text "  Tables: audit.project_log, tut.projects, tut.task_events, tut.tasks, tut.users."
    TE.:$$: TE.Text "")
instance (ToStar (TTabDef Sch name), ToStar name) => CTabDef Sch name where
  type TTabDef Sch name = TTabDefSch name
```
