{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
module PgSchema.DML.Select.Types
  -- ( QueryParam(..), qpEmpty
  -- , CondWithPath(..), OrdWithPath(..), DistWithPath(..), LimOffWithPath(..)
  -- , LO(..)
  -- , CondMonad, qRoot, qPath, qWhere, qOrderBy, qDistinct, qDistinctOn, qLimit, qOffset
  -- , Cond(..), pnull, pchild, pparent, pnot, pin, pinArr, pUnsafeCond
  -- , (|||), (&&&), (<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?), showCmp, BoolOp(..)
  -- , TabParam(..), OrdFld(..), Dist(..), defTabParam, defLO, lo1
  -- , OrdDirection(..), ascf, descf, ordf
  -- , SomeToField(..)
  -- )
  where

import Control.Monad.RWS
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Kind
import Data.List as L
import Data.List.NonEmpty as NE
import Data.String
import Data.Text(Text)
import Database.PostgreSQL.Simple.ToField
import PgSchema.Schema
import PgSchema.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeLits
import PgSchema.Utils.Internal

import Prelude.Singletons


-- | Parameters that is used to describe @SELECT@.
--
-- You don't need to make it directly. Use 'MonadQP' to define 'QueryParam' instead.
--
data QueryParam sch t = QueryParam
  { qpConds     :: ![CondWithPath sch t]    -- ^ `where` conditions for branches of Data-Tree
  , qpOrds      :: ![OrdWithPath sch t]     -- ^ `order by` clauses
  , qpLOs       :: ![LimOffWithPath sch t]  -- ^ `limit/offset` clauses
  , qpDistinct  :: ![DistWithPath sch t]    -- ^ `destinct` and `distinct on` clauses
  }

-- | Empty 'QueryParam'.
--
-- It means that @SELECT@ is defined only by structure of output type
qpEmpty :: forall sch t. QueryParam sch t
qpEmpty = QueryParam [] [] [] []

type MonadQP sch t path = (TabPath sch t path, ToStar path) => RWS (Proxy path) () (QueryParam sch t) ()

-- | Execute 'MonadQP' and get 'QueryParam'.
--
-- The table `t` define a context and become the "current" table
qRoot :: RWS (Proxy '[]) () (QueryParam sch t) () -> QueryParam sch t
qRoot m = fst $ execRWS m Proxy qpEmpty

-- | Change context (current table) to parent or child table.
--
-- 'Symbol' parameter must be the name of reference constraint "to" or "from" current table
--
qPath :: forall sch t path path'.
  forall (p :: Symbol) ->
  (TabPath sch t path', ToStar path', path' ~ path ++ '[p]) =>
  MonadQP sch t path' -> MonadQP sch t path
qPath _p m = do
  s <- get
  put $ fst $ execRWS m Proxy s

-- | Add @WHERE@ condition for the current table.
--
-- If several 'qWhere' exists they are composed in accordance to 'Monoid' for 'Cond', i.e. with '(&&&)'
qWhere :: forall sch t path. Cond sch (TabOnPath sch t path) -> MonadQP sch t path
qWhere c = modify \qp -> qp { qpConds = CondWithPath @path c : qp.qpConds }

-- | Add @ORDER BY@ condition for the current table
--
-- Several 'qOrderBy' are possible and they are composed together.
--
-- There can be mixed with "parent" table. I.e. having
--
-- @
-- qOrderBy [ascf "f1"]
-- qPath "ref-to-parent" do
--   qOrderBy [descf "f2"]
-- qOrderBy [ascf "f3"]
-- @
--
-- we get @ORDER BY t1.f1, t2.f2 DESC, t1.f3@
--
qOrderBy :: forall sch t path. [OrdFld sch (TabOnPath sch t path)] -> MonadQP sch t path
qOrderBy ofs = modify \qp -> qp { qpOrds = OrdWithPath @path ofs : qp.qpOrds }

-- | Add `DISTINCT` condition for the current table.
-- It is applied only to "root" or "children" tables.
qDistinct :: forall sch t path t'. TabOnPath2 sch t path ~ '(t', 'RelMany) =>
  MonadQP sch t path
qDistinct = modify \qp -> qp { qpDistinct = DistWithPath @path Distinct : qp.qpDistinct }

-- | Add @DISTINCT ON@ condition for the current table.
--
-- It can be applied also to "parent" table because it just joined to the current table.
--
-- 'qDistinctOn' automatically add fields to the @ORDER BY@ clause (as PostgreSql require).
-- (That's why 'OrdDirection' is needed)
-- So having
--
-- @
-- qOrderBy [descf "f0"]
-- qDistinctOn [ascf "f1"]
-- qPath "ref-to-parent" do
--   qDistinctOn [descf "f2"]
-- qDistinctOn [ascf "f3"]
-- @
--
-- we get @DISTINCT ON (t1.f1, t2.f2, t1.f3) ... ORDER BY t1.f1, t2.f2 DESC, t1.f3, t1.f0 DESC@
--
qDistinctOn :: forall sch t path. [OrdFld sch (TabOnPath sch t path)] -> MonadQP sch t path
qDistinctOn ofs = modify \qp -> qp { qpDistinct = DistWithPath @path (DistinctOn ofs) : qp.qpDistinct }

-- | Add `LIMIT` condition for the current table.
-- It is applied only to "root" or "children" tables.
--
-- If several 'qLimit` applied to the same path only the last one is used
qLimit :: forall sch t path. Snd (TabOnPath2 sch t path) ~ RelMany
  => Natural -> MonadQP sch t path
qLimit n = modify \qp -> qp { qpLOs = mk qp.qpLOs }
  where
    mk xs = case L.break eq xs of
      (xs1, []) -> new : xs1
      (xs1, x:xs2) -> xs1 <> [upd x] <> xs2
    eq (LimOffWithPath @p _) = demote @p == demote @path
    upd (LimOffWithPath @p lo) = LimOffWithPath @p lo{ limit = Just n }
    new = LimOffWithPath @path LO { limit = Just n, offset = Nothing }

-- | Add `OFFSET` condition for the current table.
-- It is applied only to "root" or "children" tables.
--
-- If several 'qOffset` applied to the same path only the last one is used
qOffset :: forall sch t path. Snd (TabOnPath2 sch t path) ~ RelMany
  => Natural -> MonadQP sch t path
qOffset n = modify \qp -> qp { qpLOs = mk qp.qpLOs }
  where
    mk xs = case L.break eq xs of
      (xs1, []) -> new : xs1
      (xs1, x:xs2) -> xs1 <> [upd x] <> xs2
    eq (LimOffWithPath @p _) = demote @p == demote @path
    upd (LimOffWithPath @p lo) = LimOffWithPath @p lo{offset = Just n}
    new = LimOffWithPath @path LO { offset = Just n, limit = Nothing }

-- | GADT to safely set `where` condition
data CondWithPath sch t where
  CondWithPath ::  forall (path :: [Symbol]) sch t. ToStar path
    => Cond sch (TabOnPath sch t path) -> CondWithPath sch t

-- | GADT to safely set `order by` clauses
data OrdWithPath sch t where
  OrdWithPath :: forall (path :: [Symbol]) sch t. ToStar path
    => [OrdFld sch (TabOnPath sch t path)] -> OrdWithPath sch t

-- | GADT to safely set `distinct/distinct on` clauses
data DistWithPath sch t where
  DistWithPath :: forall (path :: [Symbol]) sch t. ToStar path
    => Dist sch (TabOnPath sch t path) -> DistWithPath sch t

-- | GADT to safely set `limit/offset` clauses
data LimOffWithPath sch t where
  LimOffWithPath :: forall (path :: [Symbol]) sch t.
    (TabPath sch t path, ToStar path, Snd (TabOnPath2 sch t path) ~ 'RelMany)
    => LO -> LimOffWithPath sch t

-- | Comparing constructors. They are paired with corresponding operation.
-- E.g. constructor '(:=)' is paired with '(=?)' etc
data Cmp = (:=) | (:<=) | (:>=) | (:>) | (:<) | Like | ILike
  deriving (Show, Eq, Generic)

instance FromJSON Cmp
instance ToJSON Cmp

-- | Just boolean operations
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

type CDBField sch tab fld fd = (CDBFieldInfo sch tab fld
  , TDBFieldInfo sch tab fld ~ 'RFPlain fd )

type CDBValue sch tab fld fd v =
  (CDBField sch tab fld fd, ToField v, Show v, CanConvert sch tab fld fd v)

type CDBFieldNullable sch tab fld fd =
  ( CDBField sch tab fld fd, FdNullable fd ~ 'True)

-- | GADT to safely set `where` condition for table `tab` based on definition of schema `sch`
--
-- 'Cond' is 'Monoid' with conjunction ('(&&&)') as 'mappend'
--
data Cond (sch::Type) (tab::NameNSK) where
  EmptyCond :: Cond sch tab
  -- ^ Empty Condition. Neutral for conjunction '(&&&)' and disjunction '(|||)'.
  Cmp :: forall fld v sch tab fd. CDBValue sch tab fld fd v => Cmp -> v -> Cond sch tab
  -- ^ Comparing field value with parameter
  In :: forall fld v sch tab fd. CDBValue sch tab fld fd v => NonEmpty v -> Cond sch tab
  -- ^ Check that field value belongs to non-empty list of values
  InArr :: forall fld v sch tab fd. CDBValue sch tab fld fd v => [v] -> Cond sch tab
  -- ^ Check that field value belongs to the list of values
  -- if list of values is empty it return @false@
  Null :: forall fld sch tab fd. CDBFieldNullable sch tab fld fd => Cond sch tab
  -- ^ Check that field value is @NULL@
  Not :: Cond sch tab -> Cond sch tab
  -- ^ Boolean @NOT@
  BoolOp :: BoolOp -> Cond sch tab -> Cond sch tab -> Cond sch tab
  -- ^ Conjunction and disjunction
  Child :: forall sch ref. CRelDef sch ref =>
    TabParam sch (RdFrom (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
    -> Cond sch (RdTo (TRelDef sch ref))
  -- ^ condition @EXISTS@ in child table. 'TabParam' is used to limit
  -- child dataset (usually with @ORDER BY@ and @LIMIT@) before applying
  -- condition on child table
  Parent :: forall sch ref . CRelDef sch ref =>
    Cond sch (RdTo (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
  -- ^ @JOIN@ with those parents who satisfied their conditions
  UnsafeCond :: CondMonad Text -> Cond sch tab
  -- ^ Unsafe condition that user can make in 'CondMonad' by self

-- Conjunction (&&&) is much more often operation for query conditions so
-- we use it for Semigroup.
-- But note that EmptyCond is neutral also for disjuction (|||).
instance Semigroup (Cond sch tab) where
  c1 <> c2 = c1 &&& c2
  -- ^ Using conjunction ('(&&&)') for 'Semifroup' instance

instance Monoid (Cond sch tab) where
  mempty = EmptyCond

-- | Parameters for child table.
--
-- It is used to limit child dataset (usually with @ORDER BY@ and @LIMIT@) before applying
-- condition on child table
--
data TabParam sch tab = TabParam
  { cond :: Cond sch tab
  , order :: [OrdFld sch tab]
  , lo :: LO }

-- | Default empty 'TabParam'.
defTabParam :: TabParam sch tab
defTabParam = TabParam mempty mempty defLO

-- | Check that field value is @NULL@
{-# INLINE pnull #-}
pnull :: forall sch tab fd. forall name -> CDBFieldNullable sch tab name fd => Cond sch tab
pnull name = Null @name

-- | Check that exists related records in child table and the condition is satisfied there
--
-- 'TabParam' is used to limit child dataset (usually with @ORDER BY@ and @LIMIT@) before applying
-- condition on child table
--
{-# INLINE pchild #-}
pchild :: forall sch . forall ref -> CRelDef sch ref =>
  TabParam sch (RdFrom (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
  -> Cond sch (RdTo (TRelDef sch ref))
pchild name = Child @sch @name

-- | Check that condition is satisfied in parent table
{-# INLINE pparent #-}
pparent :: forall sch. forall ref -> CRelDef sch ref =>
  Cond sch (RdTo (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
pparent name = Parent @sch @name

-- | Boolean @NOT@
{-# INLINE pnot #-}
pnot :: Cond sch tab -> Cond sch tab
pnot = Not

{-# INLINE pUnsafeCond #-}
pUnsafeCond :: CondMonad Text -> Cond sch tab
pUnsafeCond = UnsafeCond

-- | Check that field value belongs to non-empty list of values
{-# INLINE pin #-}
pin :: forall name -> forall sch tab fd v. CDBValue sch tab name fd v
  => NonEmpty v -> Cond sch tab
pin name = In @name

-- | Check that field value belongs to the list of values.
-- If list of values is empty it returns @false@
{-# INLINE pinArr #-}
pinArr :: forall name -> forall sch tab fd v. CDBValue sch tab name fd v
  => [v] -> Cond sch tab
pinArr name = InArr @name

-- | Conjunction
(&&&) :: Cond sch tab -> Cond sch tab -> Cond sch tab
-- | Disjunction
(|||) :: Cond sch tab -> Cond sch tab -> Cond sch tab
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
(<?),(>?),(<=?),(>=?),(=?)
   :: forall fld -> forall sch tab fd v. CDBValue sch tab fld fd v => v -> Cond sch tab
x <? b  = Cmp @x (:<)  b
x >? b  = Cmp @x (:>)  b
x <=? b = Cmp @x (:<=) b
x >=? b = Cmp @x (:>=) b
x =? b = Cmp @x (:=) b
(~=?),(~~?)
  :: forall fld -> forall sch tab fd v. CDBValue sch tab fld fd v => v -> Cond sch tab
x ~=? b  = Cmp @x Like b
x ~~? b  = Cmp @x ILike b
infix 4 <?, >?, <=?, >=?, =?, ~=?, ~~?

data OrdDirection = Asc | Desc deriving Show

data OrdFld sch tab where
  OrdFld :: forall fld sch tab fd. CDBField sch tab fld fd =>
    OrdDirection -> OrdFld sch tab
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
  :: forall fld -> forall sch tab fd. CDBField sch tab fld fd
  => OrdDirection -> OrdFld sch tab
ordf fld = OrdFld @fld

{-# INLINE ascf #-}
ascf :: forall fld -> forall sch tab fd. CDBField sch tab fld fd => OrdFld sch tab
ascf fld = ordf fld Asc

{-# INLINE descf #-}
descf :: forall fld -> forall sch tab fd. CDBField sch tab fld fd => OrdFld sch tab
descf fld = ordf fld Desc

data LO = LO
  { limit  :: Maybe Natural
  , offset :: Maybe Natural }
  deriving Show

defLO :: LO
defLO = LO Nothing Nothing

lo1 :: LO
lo1 = LO (Just 1) Nothing
