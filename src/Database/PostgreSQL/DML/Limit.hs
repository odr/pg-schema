{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Limit where

import Control.Monad
import Data.List as L
import Data.Maybe
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Singletons.TH
import Data.Text as T
import Database.Schema.Def
import GHC.Natural
import GHC.TypeLits
import Util.ToStar


singletons [d|
  data LO' t = LO
    { limit  :: Maybe t
    , offset :: Maybe t }
    deriving Show
  |]

type LOK = LO' Nat
type LO  = LO' Natural

data LimOff sch tab = forall (lo :: LOK). ToStar lo => LimOff (Proxy lo)

data LimOffWithPath sch t
  = forall (path :: [Symbol]). ToStar path
  => LimOffWithPath (Proxy path) (LimOff sch (TabOnPath sch t path))

withLOWithPath
  :: forall sch t r. CSchema sch
  => (forall (t' :: Symbol). LimOff sch t' -> r)
  -> [Text] -> LimOffWithPath sch t -> Maybe r
withLOWithPath f path (LimOffWithPath (Proxy :: Proxy p) lo) =
  guard (path == toStar @_ @p) >> pure (f lo)

withLOsWithPath
  :: forall sch t r. CSchema sch
  => (forall (t' :: Symbol). LimOff sch t' -> r)
  -> [Text] -> [LimOffWithPath sch t] -> Maybe r
withLOsWithPath f path = join . L.find isJust . L.map (withLOWithPath f path)

lowp
  :: forall (path::[Symbol]) (lo::LOK) sch t
  . (CSchema sch, ToStar path, ToStar lo)
  => LimOffWithPath sch t
lowp = LimOffWithPath (Proxy @path) (LimOff (Proxy @lo))

rootLO
  :: forall (lo::LOK) sch t. (CSchema sch, ToStar lo) => LimOffWithPath sch t
rootLO = lowp @'[] @lo

convLO :: LimOff sch t -> Text
convLO (LimOff (Proxy::Proxy lo)) =
  fromMaybe "" ((" limit " <>) . T.pack . show <$> ml)
   <> fromMaybe "" ((" offset " <>) . T.pack . show <$> mo)
  where
    LO ml mo = toStar @_ @lo
