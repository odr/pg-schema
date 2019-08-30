{-# LANGUAGE CPP #-}
module Util.ToStar where

import Data.Singletons.Prelude


type ToStar a = (SingKind (KindOf a), SingI a)

toStar :: forall a. (SingI a, SingKind (KindOf a)) => Demote (KindOf a)
#if MIN_VERSION_base(4,12,0)
toStar = demote @a
#else
toStar = fromSing (sing :: Sing a)
#endif
