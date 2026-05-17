```haskell
newtype PgTag s t = PgTag { unPgTag :: t }
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

-- Show / Read: for s :: Symbol use "field" =: value; otherwise
-- PgTag {unPgTag = …} (see PgSchema.Types).

type s := t = PgTag s t
infixr 5 :=

(=:) :: forall b. forall a -> b -> a := b
(=:) _ = coerce
infixr 5 =:
```
