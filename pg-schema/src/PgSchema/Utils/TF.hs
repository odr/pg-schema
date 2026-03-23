{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PgSchema.Utils.TF where

-- import Data.Singletons
import Data.Singletons.TH
import GHC.TypeLits

import Data.List.Singletons as SP


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

genDefunSymbols [''NatToSymbol]

-- >>> :kind! NatToSymbol 123
-- NatToSymbol 123 :: Symbol
-- = "123"

type (++) xs ys = (SP.++) xs ys
-- type family (++) (xs :: [a]) (ys :: [a]) :: [a] where
--   (++) '[] ys = ys
--   (++) (x ': xs) ys = x ': (xs ++ ys)

type Null xs = SP.Null xs
-- | @'True@ iff the list is empty (cf. @Null@ on lists in @singletons-base@).
-- type family Null (xs :: [a]) :: Bool where
--   Null '[] = 'True
--   Null (x ': xs) = 'False

type Length xs = SP.Length xs
-- type family Length (xs :: [a]) :: Nat where
--   Length '[] = 0
--   Length (x ': xs) = 1 + Length xs

-- >>> :kind! Length '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- Length '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] :: Natural
-- = 20

type SplitAt n xs = SP.SplitAt n xs
-- type family SplitAt (n :: Nat) (xs :: [k]) :: ([k], [k]) where
--   SplitAt 0 xs             = '( '[], xs)
--   SplitAt n '[]            = '( '[], '[])
--   -- Используем вспомогательный тип, чтобы "пробросить" результат рекурсии
--   SplitAt n (x ': xs)      = SplitAtHelper x (SplitAt (n - 1) xs)
-- -- Вспомогательный тип для конструирования результата
-- type family SplitAtHelper (x :: k) (res :: ([k], [k])) :: ([k], [k]) where
--   SplitAtHelper x '(left, right) = '(x ': left, right)

-- >>> :kind! SplitAt 13 '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- SplitAt 13 '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] :: ([Natural],
--                                                                      [Natural])
-- = '( '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13],
--      '[14, 15, 16, 17, 18, 19, 20])

type Map1 f xs = SP.Map f xs
-- type family Map1 (f :: a ~> b) (xs :: [a]) :: [b] where
--   Map1 f '[] = '[]
--   Map1 f (x ': xs) = Apply f x ': Map1 f xs

type Elem' x xs = SP.Elem x xs
-- type family Elem' (x :: Symbol) (xs :: [Symbol]) :: Bool where
--   Elem' x '[] = False
--   Elem' x (x ': xs) = True
--   Elem' x (y ': xs) = Elem' x xs

-- >>> :kind! Elem' "18" (Map1 NatToSymbolSym0 '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20])
-- Elem' "18" (Map1 NatToSymbolSym0 '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]) :: Bool
-- = 'True
