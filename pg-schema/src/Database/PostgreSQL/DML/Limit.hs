module Database.PostgreSQL.DML.Limit where

import Control.Monad
import Data.List as L
import Data.Maybe
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text as T
import Database.Schema.Def
import GHC.Natural
import GHC.TypeLits
import Util.ToStar


data LO = LO
  { limit  :: Maybe Natural
  , offset :: Maybe Natural }
  deriving Show

data LimOffWithPath sch t
  = forall (path :: [Symbol]). (TabPath sch t path, ToStar path)
  => LimOffWithPath (Proxy path) LO

withLOWithPath
  :: forall sch t r. (LO -> r) -> [Text] -> LimOffWithPath sch t -> Maybe r
withLOWithPath f path (LimOffWithPath (Proxy :: Proxy p) lo) =
  guard (path == toStar @p) >> pure (f lo)

withLOsWithPath
  :: forall sch t r. (LO -> r) -> [Text] -> [LimOffWithPath sch t] -> Maybe r
withLOsWithPath f path = join . L.find isJust . L.map (withLOWithPath f path)

lowp
  :: forall (path::[Symbol]) sch t. (ToStar path, TabPath sch t path)
  => LO -> LimOffWithPath sch t
lowp = LimOffWithPath (Proxy @path)

rootLO :: forall sch t. LO -> LimOffWithPath sch t
rootLO = lowp @'[]

convLO :: LO -> Text
convLO (LO ml mo) =
  fromMaybe "" ((" limit " <>) . T.pack . show <$> ml)
   <> fromMaybe "" ((" offset " <>) . T.pack . show <$> mo)

loByPath :: forall sch t. [Text] -> [LimOffWithPath sch t] -> Text
loByPath path = fromMaybe mempty . withLOsWithPath convLO path
