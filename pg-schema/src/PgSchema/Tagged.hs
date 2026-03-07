{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module PgSchema.Tagged where

import Data.Coerce
import Data.Tagged


type s := t = Tagged s t
infixr 5 :=

(=:) :: forall b. forall a -> b -> a := b
(=:) _ = coerce
infixr 5 =:
