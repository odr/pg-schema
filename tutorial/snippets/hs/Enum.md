```haskell
data instance PGEnum Sch ( "tut" ->> "project_status" )
  = Project_status_draft | Project_status_active | Project_status_archived
  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)
```
