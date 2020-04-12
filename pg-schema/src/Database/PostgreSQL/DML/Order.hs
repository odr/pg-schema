module Database.PostgreSQL.DML.Order where

import Control.Monad
import Data.List as L
import Data.Maybe
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Singletons
import Data.Text as T
import Database.Schema.Def
import GHC.TypeLits

import PgSchema.Util


data OrdDirection = Asc | Desc deriving Show

data OrdFld sch tab
  = forall fld. CFldDef sch tab fld => OrdFld (Proxy fld) OrdDirection

ordf :: forall fld sch tab. CFldDef sch tab fld => OrdDirection -> OrdFld sch tab
ordf = OrdFld (Proxy @fld)

ascf :: forall fld sch tab. CFldDef sch tab fld => OrdFld sch tab
ascf = ordf @fld Asc

descf :: forall fld sch tab. CFldDef sch tab fld => OrdFld sch tab
descf = ordf @fld Desc

data OrdWithPath sch t
  = forall (path :: [Symbol]). ToStar path
  => OrdWithPath (Proxy path) [OrdFld sch (TabOnPath sch t path)]

withOrdWithPath
  :: forall sch t r
  . (forall t'. [OrdFld sch t'] -> r)
  -> [Text] -> OrdWithPath sch t -> Maybe r
withOrdWithPath f path (OrdWithPath (Proxy :: Proxy p) ord) =
  f ord <$ guard (path == demote @p)
--
withOrdsWithPath
  :: forall sch t r
  . (forall t'. [OrdFld sch t'] -> r)
  -> [Text] -> [OrdWithPath sch t] -> Maybe r
withOrdsWithPath f path = join . L.find isJust . L.map (withOrdWithPath f path)

owp
  :: forall path sch t t'. (ToStar path, TabOnPath sch t path ~ t')
  => [OrdFld sch t'] -> OrdWithPath sch t
owp = OrdWithPath (Proxy @path)

rootOrd :: forall sch t. [OrdFld sch t] -> OrdWithPath sch t
rootOrd = owp @'[]

convOrd :: Int -> [OrdFld sch t] -> Text
convOrd (show' -> n) ofs = T.intercalate "," $ L.map showFld ofs
  where
    showFld (OrdFld (Proxy :: Proxy fld) (show' -> od)) =
      "t" <> n <> "." <> (demote @fld) <> " " <> od

ordByPath :: forall sch t. Int -> [Text] -> [OrdWithPath sch t] -> Text
ordByPath num path = fromMaybe mempty . withOrdsWithPath (convOrd num) path
