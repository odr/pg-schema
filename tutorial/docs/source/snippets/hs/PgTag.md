```haskell
newtype PgTag s t = PgTag { unPgTag :: t }
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

type s := t = PgTag s t
infixr 5 :=

(=:) :: forall b. forall a -> b -> a := b
(=:) _ = coerce
infixr 5 =:
```
