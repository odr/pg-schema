```haskell
data GenNames = GenNames
  { schemas :: [Text]   -- ^ generate data for all tables in these schemas
  , tables  :: [NameNS] -- ^ generate data for these tables
  , addRelations :: [AddRelation] -- ^ additional relations. Be careful!
  }
```
