{-# LANGUAGE UndecidableInstances #-}
module PgSchema.HList.Internal where

import Data.Kind
import GHC.TypeLits
import PgSchema.Schema


-- | Add 1 to n when symbols equal (for renumbering).
type family AddIfSymEq (a :: Symbol) (b :: Symbol) (n :: Nat) :: Nat where
  AddIfSymEq s s n = 1 + n
  AddIfSymEq s s' n = n

-- | Count how many keys in list have the given symbol.
type family CountSymIn (acc :: [(SymNat, Type)]) (s :: Symbol) :: Nat where
  CountSymIn '[] s = 0
  CountSymIn ('( '(s',n), _) ': rest) s = AddIfSymEq s' s (CountSymIn rest s)

-- | Count how many keys in [SymNat] have the given symbol (for NormalizeGo).
type family CountSymInKeys (keys :: [SymNat]) (s :: Symbol) :: Nat where
  CountSymInKeys '[] s = 0
  CountSymInKeys ('(s',n) ': rest) s = AddIfSymEq s' s (CountSymInKeys rest s)

-- | NormalizeGo prefix xs = renumbered tail when prefix = keys emitted so far.
type family NormalizeGo (prefix :: [SymNat]) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  NormalizeGo prefix '[] = '[]
  NormalizeGo prefix ('( '(s,n), t) ': rest) =
    '( '(s, CountSymInKeys prefix s), t)
    ': NormalizeGo ('(s,n) ': prefix) rest

-- | Normalize: renumber Nats per symbol 0, 1, 2, ...
type family Normalize (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Normalize xs = NormalizeGo '[] xs

-- | Count occurrences of symbol @s@ in HList list (for unique Nat in SymNat).
type family CountName (s :: Symbol) (xs :: [(SymNat, Type)]) :: Nat where
  CountName s '[] = 0
  CountName s ('( '(s, _), _) ': xs) = 1 + CountName s xs
  CountName s (_ ': xs) = CountName s xs
