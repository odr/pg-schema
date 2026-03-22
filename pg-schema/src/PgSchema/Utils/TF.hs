{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PgSchema.Utils.TF where

import Data.Singletons
import GHC.TypeLits


-- | Second component of a type-level pair (replaces "Prelude.Singletons" @Snd@).
type family Fst (xs :: (a,b)) :: a where
  Fst '(x,_) = x

type family Snd (xs :: (a, b)) :: b where
  Snd '(_, y) = y

type family NatToSymbol (n :: Nat) :: Symbol where
  NatToSymbol 0 = "0"
  NatToSymbol 1 = "1"
  NatToSymbol 2 = "2"
  NatToSymbol 3 = "3"
  NatToSymbol 4 = "4"
  NatToSymbol 5 = "5"
  NatToSymbol 6 = "6"
  NatToSymbol 7 = "7"
  NatToSymbol 8 = "8"
  NatToSymbol 9 = "9"
  NatToSymbol n = AppendSymbol (NatToSymbol (Div n 10)) (NatToSymbol (Mod n 10))

-- >>> :kind! NatToSymbol 123
-- NatToSymbol 123 :: Symbol
-- = "123"

type family (++) (xs :: [a]) (ys :: [a]) :: [a] where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

-- | @'True@ iff the list is empty (cf. @Null@ on lists in @singletons-base@).
type family Null (xs :: [a]) :: Bool where
  Null '[] = 'True
  Null (x ': xs) = 'False

type family Length (xs :: [a]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type family SplitAt (n :: Nat) (xs :: [k]) :: ([k], [k]) where
  SplitAt 0 xs             = '( '[], xs)
  SplitAt n '[]            = '( '[], '[])
  -- Используем вспомогательный тип, чтобы "пробросить" результат рекурсии
  SplitAt n (x ': xs)      = SplitAtHelper x (SplitAt (n - 1) xs)

-- Вспомогательный тип для конструирования результата
type family SplitAtHelper (x :: k) (res :: ([k], [k])) :: ([k], [k]) where
  SplitAtHelper x '(left, right) = '(x ': left, right)

-- >>> :kind! SplitAt 2 '[1,2,3,4,5]
-- SplitAt 2 '[1,2,3,4,5] :: ([Natural], [Natural])
-- = '( '[1, 2], '[3, 4, 5])

type family Map1 (f :: a ~> b) (xs :: [a]) :: [b] where
  Map1 f '[] = '[]
  Map1 f (x ': xs) = Apply f x ': Map1 f xs
