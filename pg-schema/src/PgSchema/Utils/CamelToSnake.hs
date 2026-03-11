{-# LANGUAGE UndecidableInstances #-}
module PgSchema.Utils.CamelToSnake(type CamelToSnake) where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality


-- | CamelToSnake:
-- >>> import Data.Proxy
-- >>> symbolVal (Proxy @(CamelToSnake "TESTCamelTo_snake_Case"))
-- "t_e_s_t_camel_to_snake__case"
type CamelToSnake (s :: Symbol) = CamelToSnakeInternal (UnconsSymbol s)

type family CamelToSnakeInternal (m :: Maybe (Char, Symbol)) :: Symbol where
  CamelToSnakeInternal 'Nothing = ""
  CamelToSnakeInternal ('Just '(c, s)) =
    AppendSymbol (ConsSymbol (ToLower c) "") (SnakeLoop (UnconsSymbol s))

type family SnakeLoop (m :: Maybe (Char, Symbol)) :: Symbol where
  SnakeLoop 'Nothing = ""
  SnakeLoop ('Just '(c, s)) =
    AppendSymbol (ProcessChar c) (SnakeLoop (UnconsSymbol s))

type ProcessChar c = If (IsUpper c)
  (AppendSymbol "_" (ConsSymbol (ToLower c) ""))
  (ConsSymbol c "")

type IsUpper (c :: Char) = Not ((CmpChar c 'A' == 'LT) || (CmpChar c 'Z' == 'GT))

type ToLower (c :: Char) = If (IsUpper c) (NatToChar (CharToNat c + 32)) c
