module PgSchema.Util where

import Data.List as L
import Data.Singletons
import Data.String
import Data.Text as T


fromText :: IsString t => Text -> t
fromText = fromString . T.unpack

show' :: (IsString b, Show a) => a -> b
show' = fromString . show

intercalate' :: Monoid a => a -> [a] -> a
intercalate' a = mconcat . L.intersperse a

type ToStar a = (SingKind (KindOf a), SingI a)
