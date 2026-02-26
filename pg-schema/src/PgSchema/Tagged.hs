{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module PgSchema.Tagged where

import Data.Coerce
import Data.Tagged


type s := t = Tagged s t

(=:) :: forall b. forall a -> b -> a := b
(=:) _ = coerce
infixr 5 =:
