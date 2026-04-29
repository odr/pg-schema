```haskell
data MyRenamer :: Renamer

type family MyRenamerImpl (s :: Symbol) :: Symbol where
  MyRenamerImpl "projectOwner" = "projects_owner_id_fkey"
  MyRenamerImpl s = CamelToSnake s

type instance ApplyRenamer MyRenamer s = MyRenamerImpl s
```
