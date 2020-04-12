module PgSchema.Util where

import Data.Singletons
import Data.String
import Data.Text as T


fromText :: IsString t => Text -> t
fromText = fromString . T.unpack

show' :: (IsString b, Show a) => a -> b
show' = fromString . show

type ToStar a = (SingKind (KindOf a), SingI a)
