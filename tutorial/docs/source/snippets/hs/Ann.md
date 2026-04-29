```haskell
data Ann = Ann
  { annRen  :: Renamer -- ^ Renamer to convert Haskell names to database names.
  , annSch  :: Type    -- ^ Schema with tables, relations and types.
  , annDepth :: Nat
  -- ^ Depth of the nested relations. It is mostly used to prevent cycles in types.
  , annTab  :: NameNSK -- ^ Name of the root table.
  }
```
