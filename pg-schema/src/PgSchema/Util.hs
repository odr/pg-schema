{-# LANGUAGE CPP #-}
module PgSchema.Util where

import Data.List as L
import Data.Singletons
import Data.String
import Data.Text as T
import Prelude as P

#ifdef DEBUG
import Debug.Trace
#endif


fromText :: IsString t => Text -> t
fromText = fromString . T.unpack
{-# INLINE fromText #-}

show' :: (IsString b, Show a) => a -> b
show' = fromString . P.show
{-# INLINE show' #-}

intercalate' :: Monoid a => a -> [a] -> a
intercalate' a = mconcat . L.intersperse a
{-# INLINE intercalate' #-}

unlines' :: (Monoid a, IsString a) => [a] -> a
unlines' = intercalate' "\n"
{-# INLINE unlines' #-}

type ToStar a = (SingKind (KindOf a), SingI a)

trace' :: String -> a -> a
traceShow' :: Show a => a -> b -> b
#ifdef DEBUG
trace' = trace
traceShow' = traceShow
#else
trace' _ = id
traceShow' _ = id
#endif
{-# INLINE trace' #-}
{-# INLINE traceShow' #-}
