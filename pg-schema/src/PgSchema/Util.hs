{-# LANGUAGE CPP #-}
module PgSchema.Util where

import Data.List qualified as L
import Data.Singletons
import Data.String
import Data.Text as T
import GHC.TypeLits
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

newtype TextI (s::Symbol) = TextI { unTextI :: Text}
  deriving newtype (Eq, Show, Ord, IsString)

instance KnownSymbol s => Semigroup (TextI s) where
  TextI a <> TextI b = TextI $ intercalate' (fromString $ symbolVal (Proxy @s))
    $ L.filter (/= mempty) [a,b]

instance KnownSymbol s => Monoid (TextI s) where
  mempty = TextI mempty

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
