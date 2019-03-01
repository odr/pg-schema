{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Order where

import Control.Monad
import Data.Kind
import Data.List as L
import Data.Maybe
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Singletons.TH
import Data.Text as T
import Database.Schema.Def
import GHC.TypeLits
import Util.ToStar


singletons [d|
  data OrdDirection = Asc | Desc deriving Show
  |]

type family
  TOrder (sch::Type) (tab::Symbol) (flds::[(Symbol, OrdDirection)])
    :: Constraint where
  TOrder sch tab '[] = ()
  TOrder sch tab ('(x,od) ': xs) = (CFldDef sch tab x, TOrder sch tab xs)

data Order sch tab =
  forall flds. (ToStar flds, TOrder sch tab flds) => Order (Proxy flds)

data OrdWithPath sch t
  = forall (path :: [Symbol]). ToStar path
  => OrdWithPath (Proxy path) (Order sch (TabOnPath sch t path))

withOrdWithPath
  :: forall sch t r
  . (forall t'. Order sch t' -> r)
  -> [Text] -> OrdWithPath sch t -> Maybe r
withOrdWithPath f path (OrdWithPath (Proxy :: Proxy p) ord) =
  guard (path == toStar @_ @p) >> pure (f ord)
--
withOrdsWithPath
  :: forall sch t r
  . (forall t'. Order sch t' -> r)
  -> [Text] -> [OrdWithPath sch t] -> Maybe r
withOrdsWithPath f path = join . L.find isJust . L.map (withOrdWithPath f path)

owp
  :: forall path flds sch t
  . (TOrder sch (TabOnPath sch t path) flds, ToStar path, ToStar flds)
  => OrdWithPath sch t
owp = OrdWithPath (Proxy @path) (Order (Proxy @flds))

rootOrd
  :: forall flds sch t. (TOrder sch t flds, ToStar flds)
  => OrdWithPath sch t
rootOrd = owp @'[] @flds

convOrd :: Int -> Order sch t -> Text
convOrd (T.pack . show -> n) (Order (Proxy::Proxy fs)) =
  T.intercalate "," $ L.map showFld (toStar @_ @fs)
  where
    showFld (nm, T.pack . show -> od) = "t"<>n<>"."<>nm<>" "<>od

ordByPath :: forall sch t. Int -> [Text] -> [OrdWithPath sch t] -> Text
ordByPath num path = fromMaybe mempty . withOrdsWithPath (convOrd num) path
