{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Database.PostgreSQL.DML.Select.Types where

import Control.Monad.RWS
-- import Control.Monad.State
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Kind
import Data.List as L
import Data.List.NonEmpty as NE
import Data.String
import Data.Text(Text)
import Data.Type.Bool
import Data.Type.Equality
import Database.PostgreSQL.Convert
import Database.PostgreSQL.Simple.ToField
import Database.Schema.Def
import GHC.Generics
import GHC.Natural
import GHC.TypeLits
import GHC.TypeError qualified as TE
import PgSchema.Util

import Prelude.Singletons(type (++), demote)
import Data.Proxy


data QueryParam sch t = QueryParam
  { qpConds     :: ![CondWithPath sch t]
  , qpOrds      :: ![OrdWithPath sch t]
  , qpLOs       :: ![LimOffWithPath sch t]
  , qpDistinct  :: ![DistWithPath sch t] }

qpEmpty :: forall sch t. QueryParam sch t
qpEmpty = QueryParam [] [] [] []

{-
selectSch conn $ qRoot do
  qWhere c1
  qOrderBy ofs1
  qPath "p1" do
    qWhere c2
    qPath "p2" do
      qDistinct Distinct
    qOffset 2
  qLimit 50
-}

-- qLimit :: (TabPath sch t path, ToStar path) => Endo (Tagged path (QueryParam sch t))
-- qOffset :: (TabPath sch t path, ToStar path) => Endo (Tagged path (QueryParam sch t))

type MonadQP sch t path = (TabPath sch t path, ToStar path) => RWS (Proxy path) () (QueryParam sch t) ()

qRoot :: RWS (Proxy '[]) () (QueryParam sch t) () -> QueryParam sch t
qRoot m = fst $ execRWS m Proxy qpEmpty

qPath :: forall sch t path path'.
  forall (p :: Symbol) ->
  (TabPath sch t path', ToStar path', path' ~ path ++ '[p]) =>
  MonadQP sch t path' -> MonadQP sch t path
qPath _p m = do
  s <- get
  put $ fst $ execRWS m Proxy s

qWhere :: forall sch t path. Cond sch (TabOnPath sch t path) -> MonadQP sch t path
qWhere c = modify \qp -> qp { qpConds = CondWithPath @path c : qp.qpConds }

qOrderBy :: forall sch t path. [OrdFld sch (TabOnPath sch t path)] -> MonadQP sch t path
qOrderBy ofs = modify \qp -> qp { qpOrds = OrdWithPath @path ofs : qp.qpOrds }

qDistinct :: forall sch t path. MonadQP sch t path
qDistinct = modify \qp -> qp { qpDistinct = DistWithPath @path Distinct : qp.qpDistinct }

qDistinctOn :: forall sch t path. [OrdFld sch (TabOnPath sch t path)] -> MonadQP sch t path
qDistinctOn ofs = modify \qp -> qp { qpDistinct = DistWithPath @path (DistinctOn ofs) : qp.qpDistinct }

qLimit :: forall sch t path. Natural -> MonadQP sch t path
qLimit n = modify \qp -> qp { qpLOs = mk qp.qpLOs }
  where
    mk xs = case L.break eq xs of
      (xs1, []) -> new : xs1
      (xs1, x:xs2) -> xs1 <> [upd x] <> xs2
    eq (LimOffWithPath @p _) = demote @p == demote @path
    upd (LimOffWithPath @p lo) = LimOffWithPath @p lo{ limit = Just n }
    new = LimOffWithPath @path LO { limit = Just n, offset = Nothing }

qOffset :: forall sch t path. Natural -> MonadQP sch t path
qOffset n = modify \qp -> qp { qpLOs = mk qp.qpLOs }
  where
    mk xs = case L.break eq xs of
      (xs1, []) -> new : xs1
      (xs1, x:xs2) -> xs1 <> [upd x] <> xs2
    eq (LimOffWithPath @p _) = demote @p == demote @path
    upd (LimOffWithPath @p lo) = LimOffWithPath @p lo{offset = Just n}
    new = LimOffWithPath @path LO { offset = Just n, limit = Nothing }

data CondWithPath sch t where
  CondWithPath ::  forall (path :: [Symbol]) sch t. ToStar path
    => Cond sch (TabOnPath sch t path) -> CondWithPath sch t

data OrdWithPath sch t where
  OrdWithPath :: forall (path :: [Symbol]) sch t. ToStar path
    => [OrdFld sch (TabOnPath sch t path)] -> OrdWithPath sch t

data DistWithPath sch t where
  DistWithPath :: forall (path :: [Symbol]) sch t. ToStar path
    => Dist sch (TabOnPath sch t path) -> DistWithPath sch t

data LimOffWithPath sch t where
  LimOffWithPath :: forall (path :: [Symbol]) sch t. (TabPath sch t path, ToStar path)
    => LO -> LimOffWithPath sch t



data QueryRead sch t = QueryRead
  { qrCurrTabNum :: !Int
  , qrIsRoot     :: !Bool
  , qrPath       :: ![Text]
  , qrParam      :: !(QueryParam sch t) }

data QueryState = QueryState
  { qsLastTabNum :: !Int
  , qsJoins      :: ![Text]
  , qsHasWhere   :: !Bool
  , qsOrd        :: !Text
  , qsLimOff     :: !Text }
  deriving Show

type MonadQuery sch t m = (MonadRWS (QueryRead sch t) [SomeToField] QueryState m)

data Cmp = (:=) | (:<=) | (:>=) | (:>) | (:<) | Like | ILike
  deriving (Show, Eq, Generic)

instance FromJSON Cmp
instance ToJSON Cmp

data BoolOp = And | Or
  deriving (Show, Eq, Generic)

instance FromJSON BoolOp
instance ToJSON BoolOp

showCmp :: IsString s => Cmp -> s
showCmp = \case
  (:=)  -> "="
  (:<=) -> "<="
  (:>=) -> ">="
  (:<)  -> "<"
  (:>)  -> ">"
  Like  -> "like"
  ILike -> "ilike"

{- | RWS-Monad to generate condition.
* Read: Stack of numbers of parent tables. The top is "current table"
* Write: List of placeholder-values.

    Note: We have to generate sql from top to bottom to correct order of fields

* State: Last number of table "in use"
-}
type CondMonad = RWS (Text, NonEmpty Int) [SomeToField] Int

data SomeToField where
  SomeToField :: (ToField a, Show a) => a -> SomeToField

deriving instance Show SomeToField

instance ToField SomeToField where
  toField (SomeToField v) = toField v

-- https://github.com/emmanueljs1/ghc-proposals/blob/5a685faf899a2b00361b221d7e945a4922bf7863/existental-type-variables.rst#implementation-plan
-- we have to add Proxy to existensials while ^ this proposal isn't implemented
data Cond (sch::Type) (tab::NameNSK) where
  EmptyCond :: Cond sch tab
  Cmp :: forall fld v sch tab.
    ( CFldDef sch tab fld, ToField v, Show v
    , CanConvert sch (FdType (TFldDef sch tab fld))
      (FdNullable (TFldDef sch tab fld)) v)
    => Cmp -> v -> Cond sch tab
  In :: forall fld v sch tab.
    ( CFldDef sch tab fld, ToField v, Show v
    , CanConvert sch (FdType (TFldDef sch tab fld))
      (FdNullable (TFldDef sch tab fld)) v )
    => NonEmpty v -> Cond sch tab
  Null :: forall fld sch tab.
    (CFldDef sch tab fld, FdNullable (TFldDef sch tab fld) ~ 'True) =>
    Cond sch tab
  Not :: Cond sch tab -> Cond sch tab
  BoolOp :: BoolOp -> Cond sch tab -> Cond sch tab -> Cond sch tab
  -- condition "EXIST"
  Child :: forall ref rel sch tab from.
    ( rel ~ TRelDef sch ref, tab ~ RdTo rel, from ~ RdFrom rel
    , CTabDef sch from, CRelDef sch ref ) =>
    TabParam sch from -> Cond sch from -> Cond sch tab
  -- condition "JOIN"
  Parent :: forall ref sch.
    ( CTabDef sch (RdTo (TRelDef sch ref)) , CRelDef sch ref ) =>
    Cond sch (RdTo (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
  UnsafeCond :: CondMonad Text -> Cond sch tab

-- Conjunction (&&&) is much more often operation for query conditions so
-- we use it for Semigroup.
-- But note that EmptyCond is neutral also for disjuction (|||).
instance Semigroup (Cond sch tab) where
  c1 <> c2 = c1 &&& c2

instance Monoid (Cond sch tab) where
  mempty = EmptyCond

data TabParam sch tab = TabParam
  { cond :: Cond sch tab
  , order :: [OrdFld sch tab]
  , lo :: LO }

defTabParam :: TabParam sch tab
defTabParam = TabParam mempty mempty defLO

{-# INLINE pnull #-}
pnull :: forall sch tab. forall name ->
  (CFldDef sch tab name, FdNullable (TFldDef sch tab name) ~ 'True) =>
  Cond sch tab
pnull name = Null @name

{-# INLINE pchild #-}
pchild :: forall sch. forall name ->
  (CTabDef sch (RdFrom (TRelDef sch name)), CRelDef sch name) =>
  TabParam sch (RdFrom (TRelDef sch name)) ->
  Cond sch (RdFrom (TRelDef sch name)) -> Cond sch (RdTo (TRelDef sch name))
pchild name = Child @name

{-# INLINE pparent #-}
pparent :: forall sch. forall ref ->
  ( CTabDef sch (RdTo (TRelDef sch ref)) , CRelDef sch ref ) =>
  Cond sch (RdTo (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
pparent name = Parent @name @sch

{-# INLINE pnot #-}
pnot :: Cond sch tab -> Cond sch tab
pnot = Not

{-# INLINE pUnsafeCond #-}
pUnsafeCond :: CondMonad Text -> Cond sch tab
pUnsafeCond = UnsafeCond

{-# INLINE pin #-}
pin :: forall name -> forall sch tab v.
  ( CFldDef sch tab name, Show v, ToField v
  , CanConvert sch (FdType (TFldDef sch tab name))
    (FdNullable (TFldDef sch tab name)) v ) =>
  NonEmpty v -> Cond sch tab
pin name = In @name

(&&&), (|||) :: Cond sch tab -> Cond sch tab -> Cond sch tab
EmptyCond &&& cond = cond
cond &&& EmptyCond = cond
c1 &&& c2 = BoolOp And c1 c2
EmptyCond ||| cond = cond
cond ||| EmptyCond = cond
c1 ||| c2 = BoolOp Or c1 c2
infixl 2 |||
infixl 3 &&&
--
{-# INLINE (<?) #-}
{-# INLINE (>?) #-}
{-# INLINE (<=?) #-}
{-# INLINE (>=?) #-}
{-# INLINE (=?) #-}
{-# INLINE (~=?) #-}
{-# INLINE (~~?) #-}
(<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?) :: forall fld -> forall sch tab v.
  ( CFldDef sch tab fld, ToField v, Show v
  , CanConvert sch (FdType (TFldDef sch tab fld))
    (FdNullable (TFldDef sch tab fld)) v) =>
  v -> Cond sch tab
x <? b  = Cmp @x (:<)  b
x >? b  = Cmp @x (:>)  b
x <=? b = Cmp @x (:<=) b
x >=? b = Cmp @x (:>=) b
x =? b = Cmp @x (:=) b
x ~=? b  = Cmp @x Like b
x ~~? b  = Cmp @x ILike b
infix 4 <?, >?, <=?, >=?, =? --, ~=?, ~~?

data OrdDirection = Asc | Desc deriving Show

type RelTab2 sch rel tab = If (RdTo (TRelDef sch rel) == tab)
  (RdFrom (TRelDef sch rel))
  (If (RdFrom (TRelDef sch rel) == tab)
    (RdTo (TRelDef sch rel))
    (TypeError (TE.Text "Relation " :<>: TE.ShowType rel
      :<>: TE.Text " is not connected to table " :<>: TE.ShowType tab)))

data OrdFld sch tab where
  OrdFld :: forall fld sch tab. CFldDef sch tab fld =>
    OrdDirection -> OrdFld sch tab
  -- SelFld :: forall (rel :: NameNSK) (fld :: Symbol) sch tab. CRelDef sch rel =>
  --   Cond sch (RelTab2 sch rel tab) -> [OrdFld sch (RelTab2 sch rel tab)] ->
  --   OrdDirection -> OrdFld sch tab
  UnsafeOrd :: CondMonad (Text, OrdDirection) -> OrdFld sch tab

data Dist sch tab where
  Distinct :: Dist sch tab
  -- | Having 'DistinctOn` we automatically add fields from DistinctOn
  -- into the begining of ORDER BY.
  -- (It is "good enough" and more simple than check it on type level).
  --
  -- That's why we use 'OrdFld' who include 'OrdDirection'.
  -- Naturally 'OrdDirection' is not used in DISTINCT ON part itself.
  --
  -- Beside that DISTINCT ON part can include expressions like ORDER BY.
  -- We can also use 'UnsafeOrd' here
  DistinctOn :: [OrdFld sch tab] -> Dist sch tab

{-# INLINE ordf #-}
ordf
  :: forall fld
  -> forall sch tab. CFldDef sch tab fld
  => OrdDirection -> OrdFld sch tab
ordf fld = OrdFld @fld

{-# INLINE ascf #-}
ascf :: forall fld -> forall sch tab. CFldDef sch tab fld => OrdFld sch tab
ascf fld = ordf fld Asc

{-# INLINE descf #-}
descf :: forall fld -> forall sch tab. CFldDef sch tab fld => OrdFld sch tab
descf fld = ordf fld Desc

data LO = LO
  { limit  :: Maybe Natural
  , offset :: Maybe Natural }
  deriving Show

defLO :: LO
defLO = LO Nothing Nothing

lo1 :: LO
lo1 = LO (Just 1) Nothing
