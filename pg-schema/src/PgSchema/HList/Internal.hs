{-# LANGUAGE UndecidableInstances #-}
module PgSchema.HList.Internal where

import Data.Kind
import GHC.TypeLits
import PgSchema.Schema
import Prelude.Singletons( type (++) )


type family AddNum (xs :: [(SymNat, Type)]) (cnts :: [SymNat]) (accCnts :: [SymNat]) :: [(SymNat, Type)]
  where
    AddNum '[] _ _ = '[]
    AddNum ('( '(s,_),t) : xs) '[] accCnts = '( '(s,0), t)
      ': AddNum xs ('(s,0) ': accCnts) '[]
    AddNum ('( '(s,_),t) : xs) ('(s,n) ': rest) accCnts = '( '(s,n+1), t)
      ': AddNum xs ('(s,n+1) ': accCnts ++ rest) '[]
    AddNum ('( '(s,x),t) : xs) ('(s',n) ': rest) accCnts =
      AddNum ('( '(s,x),t) : xs) rest ('(s',n) ': accCnts)

type Normalize xs = AddNum xs '[] '[]
-- >>> :kind! Normalize '[ '( '("a", 2), Int), '( '("b", 2), Int), '( '("a", 2), Char), '( '("b", 1), Int)]
-- Normalize '[ '( '("a", 2), Int), '( '("b", 2), Int), '( '("a", 2), Char), '( '("b", 1), Int)] :: [((Symbol,
--                                                                                                     Natural),
--                                                                                                    *)]
-- = '[ '( '("a", 0), Int), '( '("b", 0), Int), '( '("a", 1), Char),
--      '( '("b", 1), Int)]
