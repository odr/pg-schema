{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.Internal where

import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Database.Schema.Def


-- | Type-level append for list of types.
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Append for HListTag list (SymNat, Type).
type family AppendHList (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  AppendHList '[] ys = ys
  AppendHList (x ': xs) ys = x ': AppendHList xs ys

-- | Symbol part of SymNat.
type family SymNatSymbol (sn :: SymNat) :: Symbol where
  SymNatSymbol '(s, n) = s

-- | Add 1 to n when symbols equal (for renumbering).
type family AddIfSymEq (a :: Symbol) (b :: Symbol) (n :: Nat) :: Nat where
  AddIfSymEq s s n = 1 + n
  AddIfSymEq s s' n = n

-- | Count how many keys in list have the given symbol.
type family CountSymIn (acc :: [(SymNat, Type)]) (s :: Symbol) :: Nat where
  CountSymIn '[] s = 0
  CountSymIn ('(sn, _) ': rest) s = AddIfSymEq (SymNatSymbol sn) s (CountSymIn rest s)

-- | Count how many keys in [SymNat] have the given symbol (for NormalizeGo).
type family CountSymInKeys (keys :: [SymNat]) (s :: Symbol) :: Nat where
  CountSymInKeys '[] s = 0
  CountSymInKeys (sn ': rest) s = AddIfSymEq (SymNatSymbol sn) s (CountSymInKeys rest s)

-- | NormalizeGo prefix xs = renumbered tail when prefix = keys emitted so far.
type family NormalizeGo (prefix :: [SymNat]) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  NormalizeGo prefix '[] = '[]
  NormalizeGo prefix ('(sn, t) ': rest) =
    '( '(SymNatSymbol sn, CountSymInKeys prefix (SymNatSymbol sn)), t)
    ': NormalizeGo (sn ': prefix) rest

-- | Normalize: renumber Nats per symbol 0, 1, 2, ...
type family Normalize (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Normalize xs = NormalizeGo '[] xs

-- | Length of HListTag list.
type family Length (xs :: [(SymNat, Type)]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

-- | Take first n elements.
type family Take (n :: Nat) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Take 0 xs = '[]
  Take n '[] = '[]
  Take n (x ': xs) = x ': Take (n - 1) xs

-- | Drop first n elements.
type family Drop (n :: Nat) (xs :: [(SymNat, Type)]) :: [(SymNat, Type)] where
  Drop 0 xs = xs
  Drop n '[] = '[]
  Drop n (_ ': xs) = Drop (n - 1) xs

-- | Types only (strip keys) for retag.
type family TypesOf (xs :: [(SymNat, Type)]) :: [Type] where
  TypesOf '[] = '[]
  TypesOf ('(_, t) ': xs) = t ': TypesOf xs

type family IsMaybe (x :: Type) :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe a = 'False

type family UnMaybe (x :: Type) :: Type where
  UnMaybe (Maybe a) = a
  UnMaybe a = a

-- | Extract field name symbol from Generic selector metadata (MetaSel).
type family GSelName (m :: Meta) :: Symbol where
  GSelName (MetaSel ('Just s) u v w) = s

-- | Count occurrences of symbol @s@ in HListTag list (for unique Nat in SymNat).
type family CountName (s :: Symbol) (xs :: [(SymNat, Type)]) :: Nat where
  CountName s '[] = 0
  CountName s ('( '(s, _), _) ': xs) = 1 + CountName s xs
  CountName s (_ ': xs) = CountName s xs

-- | Type-level equality for Bool (for Assert).
type family EqBool (a :: Bool) (b :: Bool) :: Bool where
  EqBool a a = 'True
  EqBool a b = 'False
