{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
module PgSchema.DML.Select.Types
  -- ( QueryParam(..), qpEmpty
  -- , CondWithPath(..), OrdWithPath(..), DistWithPath(..), LimOffWithPath(..)
  -- , LO(..)
  -- , CondMonad, qRoot, qPath, qPathFromHere, qPathToHere, qWhere, qOrderBy, qDistinct, qDistinctOn, qLimit, qOffset
  -- , Cond(..), pnull, pchild, pparent, pnot, pin, pinArr, pUnsafeCond
  -- , (|||), (&&&), (<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?), showCmp, BoolOp(..)
  -- , TabParam(..), OrdFld(..), Dist(..), defTabParam, defLO, lo1
  -- , OrdDirection(..), ascf, descf, ordf
  -- , SomeToField(..)
  -- )
  where

import Control.Monad.RWS
import Data.Kind
import Data.Proxy
import Data.Singletons (demote)
import Data.List as L
import Data.List.NonEmpty as NE
import Data.String
import Data.Text(Text)
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import GHC.Natural
import GHC.TypeLits
import GHC.TypeError qualified as TE
import PgSchema.Ann
import PgSchema.Schema
import PgSchema.Types
import PgSchema.Utils.TF


-- | Parameters that are used to describe @SELECT@.
--
-- You don't need to make it directly. Use 'MonadQP' to define t'QueryParam' instead.
--
data QueryParam ren sch t = QueryParam
  { qpConds     :: ![CondWithPath ren sch t]    -- ^ @WHERE@ conditions for branches of the data tree
  , qpOrds      :: ![OrdWithPath ren sch t]     -- ^ @ORDER BY@ clauses
  , qpLOs       :: ![LimOffWithPath ren sch t]  -- ^ @LIMIT@ / @OFFSET@ clauses
  , qpDistinct  :: ![DistWithPath ren sch t]    -- ^ @DISTINCT@ and @DISTINCT ON@ clauses
  }

-- | t'QueryParam' for the renamer, schema and root table of annotation @ann@.
--
-- It is the type of query parameters accepted by 'PgSchema.DML.selectSch'.
type QueryParamAnn ann = QueryParam (AnnRen ann) (AnnSch ann) (AnnTab ann)

-- | Empty t'QueryParam'.
--
-- It means that @SELECT@ is defined only by the structure of the output type.
qpEmpty :: forall ren sch t. QueryParam ren sch t
qpEmpty = QueryParam [] [] [] []

-- | Monad to describe t'QueryParam' for root table @t@.
--
-- @path@ is the path from the root table to the current table (the table that
-- conditions, ordering, limits etc. are applied to). It is empty for the root
-- table and is extended by 'qPath', 'qPathFromHere' and 'qPathToHere'.
--
-- Run it with 'qRoot':
--
-- > qRoot do
-- >   qWhere $ "name" ~~? "%john%"
-- >   qPath "tasks" do
-- >     qOrderBy [descf "created_at"]
-- >     qLimit 10
type MonadQP ren sch t path
  = PathCtx ren sch t path
  => RWS (Proxy path) () (QueryParam ren sch t) ()

-- | Execute 'MonadQP' and get t'QueryParam'.
--
-- The table @t@ defines a context and becomes the "current" table.
qRoot :: RWS (Proxy '[]) () (QueryParam ren sch t) () -> QueryParam ren sch t
qRoot m = fst $ execRWS m Proxy qpEmpty

-- | Change context (current table) to parent or child table.
--
-- The 'Symbol' must name the foreign-key constraint (edge to the parent or from the child)
-- for the step away from the current table.
--
qPath :: forall ren sch t path path' tabPath p' k. forall (p :: Symbol)
  -> PathCheck 'Nothing p ren sch t path path' tabPath p' k
  => MonadQP ren sch t path' -> MonadQP ren sch t path
qPath _p m = put . fst . execRWS m Proxy =<< get

-- | Change context (current table) to parent table.
--
-- The 'Symbol' must name the foreign-key constraint (edge to the parent)
-- for the step away from the current table.
--
qPathFromHere :: forall ren sch t path path' tabPath p' k. forall (p :: Symbol)
  -> PathCheck ('Just 'FromHere) p ren sch t path path' tabPath p' k
  => MonadQP ren sch t path' -> MonadQP ren sch t path
qPathFromHere _p m = put . fst . execRWS m Proxy =<< get

-- | Change context (current table) to child table.
--
-- The 'Symbol' must name the foreign-key constraint (edge from the child)
-- for the step away from the current table.
--
qPathToHere :: forall ren sch t path path' tabPath p' k. forall (p :: Symbol)
  -> PathCheck ('Just 'ToHere) p ren sch t path path' tabPath p' k
  => MonadQP ren sch t path' -> MonadQP ren sch t path
qPathToHere _p m = put . fst . execRWS m Proxy =<< get

type PathCheck pathKind p ren sch t path path' tabPath p' k =
  ( p' ~ ApplyRenamer ren p
  , tabPath ~ TabOnDPathRen ren sch t path
  , k ~ ResolvePathKind pathKind (HasFromStep sch tabPath p') (HasToStep sch tabPath p')
      sch tabPath p p'
  , path' ~ (path ++ '[ 'PathElem p p' k ])
  , PathCtx ren sch t path'
  )
-- | Add @WHERE@ condition for the current table.
--
-- If several 'qWhere' exist they are composed according to the 'Monoid' instance for 'Cond', i.e. with '(&&&)'.
qWhere :: forall ren sch t path. Cond ren sch (TabOnDPathRen ren sch t path) -> MonadQP ren sch t path
qWhere c = modify \qp -> qp { qpConds = CondWithPath @path (demote @path) c : qp.qpConds }

-- | Add @ORDER BY@ condition for the current table.
--
-- Several 'qOrderBy' are possible and they are composed together.
--
-- Ordering can span the current table and joined parents. For example:
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
qOrderBy :: forall ren sch t path. [OrdFld ren sch (TabOnDPathRen ren sch t path)] -> MonadQP ren sch t path
qOrderBy ofs = modify \qp -> qp { qpOrds = OrdWithPath @path (demote @path) ofs : qp.qpOrds }

-- | Add @DISTINCT@ condition for the current table.
-- It is applied only to "root" or "children" tables.
qDistinct :: forall ren sch t path. PathEndsMany sch t path =>
  MonadQP ren sch t path
qDistinct = modify \qp -> qp { qpDistinct = DistWithPath @path (demote @path) Distinct : qp.qpDistinct }

-- | Add @DISTINCT ON@ condition for the current table.
--
-- It can also be applied on a parent table because that parent is joined to the current table.
--
-- 'qDistinctOn' automatically adds fields to the @ORDER BY@ clause (as PostgreSQL requires).
-- (That's why 'OrdDirection' is needed.)
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
qDistinctOn :: forall ren sch t path. [OrdFld ren sch (TabOnDPathRen ren sch t path)] -> MonadQP ren sch t path
qDistinctOn ofs =
  modify \qp -> qp { qpDistinct = DistWithPath @path (demote @path) (DistinctOn ofs) : qp.qpDistinct }

-- | Add @LIMIT@ condition for the current table.
-- It is applied only to "root" or "children" tables.
--
-- If 'qLimit' is applied several times on the same path, only the last one is used.
qLimit :: forall ren sch t path. PathEndsMany sch t path =>
  Natural -> MonadQP ren sch t path
qLimit n = modify \qp -> qp { qpLOs = mk qp.qpLOs }
  where
    mk xs = case L.break eq xs of
      (xs1, []) -> new : xs1
      (xs1, x:xs2) -> xs1 <> [upd x] <> xs2
    eq (LimOffWithPath p _) = pathsEqual p (demote @path)
    upd (LimOffWithPath @p' p lo) = LimOffWithPath @p' p lo{ limit = Just n }
    new = LimOffWithPath @path (demote @path) LO { limit = Just n, offset = Nothing }

-- | Add @OFFSET@ condition for the current table.
-- It is applied only to "root" or "children" tables.
--
-- If 'qOffset' is applied several times on the same path, only the last one is used.
qOffset :: forall ren sch t path. PathEndsMany sch t path =>
  Natural -> MonadQP ren sch t path
qOffset n = modify \qp -> qp { qpLOs = mk qp.qpLOs }
  where
    mk xs = case L.break eq xs of
      (xs1, []) -> new : xs1
      (xs1, x:xs2) -> xs1 <> [upd x] <> xs2
    eq (LimOffWithPath p _) = pathsEqual p (demote @path)
    upd (LimOffWithPath @p' p lo) = LimOffWithPath @p' p lo{offset = Just n}
    new = LimOffWithPath @path (demote @path) LO { offset = Just n, limit = Nothing }

-- | GADT to safely set @WHERE@ condition.
data CondWithPath ren sch t where
  CondWithPath :: forall (path :: [PathElemK]) ren sch t. PathCtx ren sch t path
    => [PathElem] -> Cond ren sch (TabOnDPathRen ren sch t path) -> CondWithPath ren sch t

-- | GADT to safely set @ORDER BY@ clauses.
data OrdWithPath ren sch t where
  OrdWithPath :: forall (path :: [PathElemK]) ren sch t. PathCtx ren sch t path
    => [PathElem] -> [OrdFld ren sch (TabOnDPathRen ren sch t path)] -> OrdWithPath ren sch t

-- | GADT to safely set @DISTINCT@ / @DISTINCT ON@ clauses.
data DistWithPath ren sch t where
  DistWithPath :: forall (path :: [PathElemK]) ren sch t. PathCtx ren sch t path
    => [PathElem] -> Dist ren sch (TabOnDPathRen ren sch t path) -> DistWithPath ren sch t

-- | GADT to safely set @LIMIT@ / @OFFSET@ clauses.
data LimOffWithPath ren sch t where
  LimOffWithPath :: forall (path :: [PathElemK]) ren sch t.
    ( PathCtx ren sch t path, PathEndsMany sch t path )
    => [PathElem] -> LO -> LimOffWithPath ren sch t

-- | Comparison constructors; each is paired with its corresponding operator
-- (e.g. v'(:=)' with '(=?)').
data Cmp = (:=) | (:<=) | (:>=) | (:>) | (:<) | Like | ILike | NotDistinctFrom
  deriving (Show, Eq, Generic)

-- | Boolean operations.
data BoolOp = And | Or deriving (Show, Eq, Generic)

-- | SQL operator for the comparison, e.g. @\"<=\"@ for v'(:<=)' or
-- @\"ilike\"@ for 'ILike'.
showCmp :: IsString s => Cmp -> s
showCmp = \case
  (:=)  -> "="
  (:<=) -> "<="
  (:>=) -> ">="
  (:<)  -> "<"
  (:>)  -> ">"
  Like  -> "like"
  ILike -> "ilike"
  NotDistinctFrom -> "is not distinct from"

{- | RWS monad to generate condition.

* Read: the prefix of table aliases in nested subqueries and the stack of
  numbers of the tables (the top is the "current table"). The alias of the
  current table is @t\<n\>@ or, in a subquery, @t\<parent\>\<prefix\>\<n\>@
  (see 'PgSchema.DML.tabPref').
* Write: list of placeholder values.
  SQL is generated top-down so placeholder values appear in the correct order.
* State: last number of table "in use".
-}
type CondMonad = RWS (Text, NonEmpty Int) [SomeToField] Int

-- | A value of any type with 'ToField' (and 'Show' for debugging): a value for
-- one @?@ placeholder of the generated SQL.
--
-- The list of these values is returned together with the SQL text by
-- 'PgSchema.DML.selectText', 'PgSchema.DML.updateText' etc.
-- In 'CondMonad' you add placeholder values with 'tell', e.g. in 'pUnsafeCond'.
data SomeToField where
  SomeToField :: (ToField a, Show a) => a -> SomeToField

deriving instance Show SomeToField

instance ToField SomeToField where
  toField (SomeToField v) = toField v

-- | Field @fld@ (a database name, after the renamer) is a plain column of table
-- @tab@ in schema @sch@. Otherwise it is a compile-time error.
type CDBField sch tab fld =
  (CDBField' sch tab fld (TDBFieldInfo sch tab fld), CDBFieldInfo sch tab fld)

type family CDBField' sch tab fld fi :: Constraint where
  CDBField' sch tab fld ('RFPlain fd) = ()
  CDBField' sch tab fld fi = TypeError
    (TE.Text "Field " :<>: ShowType fld :<>: TE.Text" is not found in table " :<>: ShowType tab
    :$$: TE.Text "Schema: " :<>: ShowType sch
    :$$: TE.Text "")

-- type CDBValue sch tab fld fd v =
--   (CDBField sch tab fld fd, ToField v, Show v, CanConvert sch tab fld fd v)

-- | Field @fld@ (a database name, after the renamer) is a plain column of table
-- @tab@, and a value of Haskell type @v@ can be compared with it ('CanConvert'):
-- e.g. @v@ is 'Maybe' iff the column is nullable.
type CDBValue sch tab fld v =
  (CDBValue' sch tab fld (TDBFieldInfo sch tab fld) v, CDBFieldInfo sch tab fld
  , ToField v, Show v)

type family CDBValue' sch tab fld fi v where
  CDBValue' sch tab fld ('RFPlain fd) v = (CanConvert sch tab fld fd v)
  CDBValue' sch tab fld fi v = TypeError
    (TE.Text "Field " :<>: ShowType fld :<>: TE.Text" is not found in table " :<>: ShowType tab
    :$$: TE.Text "Schema: " :<>: ShowType sch
    :$$: TE.Text "")

-- | Field @fld@ (a database name, after the renamer) is a nullable plain column
-- of table @tab@.
type CDBFieldNullable sch tab fld =
  ( CDBField sch tab fld , FdNullable (GetFldDef sch tab fld) ~ 'True)

-- | GADT to safely set @WHERE@ condition for table @tab@ based on definition of schema @sch@.
--
-- 'Cond' is 'Monoid' with conjunction ('(&&&)') as 'mappend'.
--
data Cond (ren :: Renamer) (sch::Type) (tab::NameNSK) where
  EmptyCond :: Cond ren sch tab
  -- ^ Empty Condition. Neutral for conjunction '(&&&)' and disjunction '(|||)'.
  Cmp :: forall fld v ren sch tab. CDBValue sch tab (ApplyRenamer ren fld) v => Cmp -> v -> Cond ren sch tab
  -- ^ Comparing field value with parameter.
  In :: forall fld v ren sch tab. CDBValue sch tab (ApplyRenamer ren fld) v => NonEmpty v -> Cond ren sch tab
  -- ^ Check that field value belongs to non-empty list of values.
  InArr :: forall fld v ren sch tab. CDBValue sch tab (ApplyRenamer ren fld) v => [v] -> Cond ren sch tab
  -- ^ Check that field value belongs to the list of values.
  -- If the list is empty, the condition evaluates to @false@.
  Null :: forall fld ren sch tab.
    CDBFieldNullable sch tab (ApplyRenamer ren fld) => Cond ren sch tab
  -- ^ Check that field value is @NULL@.
  Not :: Cond ren sch tab -> Cond ren sch tab
  -- ^ Boolean @NOT@.
  BoolOp :: BoolOp -> Cond ren sch tab -> Cond ren sch tab -> Cond ren sch tab
  -- ^ Conjunction and disjunction.
  Child :: forall ren sch ref. CRelDef sch (ApplyRenamerNS ren ref)
    => TabParam ren sch (RdFrom (TRelDef sch (ApplyRenamerNS ren ref)))
    -> Cond ren sch (RdFrom (TRelDef sch (ApplyRenamerNS ren ref)))
    -> Cond ren sch (RdTo (TRelDef sch (ApplyRenamerNS ren ref)))
  -- ^ Condition @EXISTS@ in child table. t'TabParam' is used to limit
  -- child dataset (usually with @ORDER BY@ and @LIMIT@) before applying
  -- condition on child table.
  Parent :: forall ren sch ref . CRelDef sch (ApplyRenamerNS ren ref)
    => Cond ren sch (RdTo (TRelDef sch (ApplyRenamerNS ren ref)))
    -> Cond ren sch (RdFrom (TRelDef sch (ApplyRenamerNS ren ref)))
  -- ^ Condition @EXISTS@ in parent table: the parent row satisfies the nested condition.
  UnsafeCond :: CondMonad Text -> Cond ren sch tab
  -- ^ Unsafe condition built manually inside 'CondMonad'.

-- | 'Cond' for the renamer, schema and root table of annotation @ann@
-- (e.g. for 'PgSchema.DML.updateByCond' and 'PgSchema.DML.deleteByCond').
type CondAnn ann = Cond (AnnRen ann) (AnnSch ann) (AnnTab ann)

-- | Conjunction ('(&&&)') is a much more frequent operation for query conditions,
-- so we use it for 'Semigroup'.
-- But note that 'EmptyCond' is also neutral for disjunction ('(|||)').
instance Semigroup (Cond ren sch tab) where
  c1 <> c2 = c1 &&& c2

instance Monoid (Cond ren sch tab) where
  mempty = EmptyCond

-- | Parameters for child table.
--
-- It is used to limit child dataset (usually with @ORDER BY@ and @LIMIT@) before applying
-- condition on child table.
--
data TabParam ren sch tab = TabParam
  { cond :: Cond ren sch tab    -- ^ Condition on child rows.
  , order :: [OrdFld ren sch tab] -- ^ Ordering of child rows (for 'lo').
  , lo :: LO                    -- ^ Limit and offset of child rows.
  }

-- | Default empty t'TabParam': no conditions, no ordering, no limits.
defTabParam :: TabParam ren sch tab
defTabParam = TabParam mempty mempty defLO

-- | Check that field value is @NULL@.
{-# INLINE pnull #-}
pnull :: forall ren sch tab. forall name -> CDBFieldNullable sch tab (ApplyRenamer ren name) => Cond ren sch tab
pnull name = Null @name

-- | True when related rows exist in the child table and satisfy the nested condition there.
--
-- t'TabParam' is used to limit child dataset (usually with @ORDER BY@ and @LIMIT@) before applying
-- condition on child table.
--
{-# INLINE pchild #-}
pchild :: forall ren sch . forall ref -> CRelDef sch (ApplyRenamerNS ren ref)
  => TabParam ren sch (RdFrom (TRelDef sch (ApplyRenamerNS ren ref)))
  -> Cond ren sch (RdFrom (TRelDef sch (ApplyRenamerNS ren ref)))
  -> Cond ren sch (RdTo (TRelDef sch (ApplyRenamerNS ren ref)))
pchild name = Child @ren @sch @name

-- | Check that condition is satisfied in parent table.
{-# INLINE pparent #-}
pparent :: forall ren sch. forall ref -> CRelDef sch (ApplyRenamerNS ren ref)
  => Cond ren sch (RdTo (TRelDef sch (ApplyRenamerNS ren ref)))
  -> Cond ren sch (RdFrom (TRelDef sch (ApplyRenamerNS ren ref)))
pparent name = Parent @ren @sch @name

-- | Boolean @NOT@.
{-# INLINE pnot #-}
pnot :: Cond ren sch tab -> Cond ren sch tab
pnot = Not

-- | Unsafe condition written as raw SQL inside 'CondMonad'.
--
-- Use it only for conditions that can't be expressed with the safe API.
-- The SQL text is not checked at all: neither the names of the columns nor the
-- types of the values.
--
-- * 'PgSchema.DML.tabPref' gives the alias of the current table and
--   'PgSchema.DML.qual' gives a column
--   qualified with it (use database names: the renamer is not applied).
-- * Add a value for each @?@ placeholder with 'tell' (in the order of the
--   placeholders).
--
-- > pUnsafeCond do
-- >   name <- qual @"name"
-- >   tell [SomeToField ("^jo" :: Text)]
-- >   pure $ name <> " ~ ?"
{-# INLINE pUnsafeCond #-}
pUnsafeCond :: CondMonad Text -> Cond ren sch tab
pUnsafeCond = UnsafeCond

-- | Check that field value belongs to non-empty list of values.
{-# INLINE pin #-}
pin :: forall name -> forall ren sch tab v. CDBValue sch tab (ApplyRenamer ren name) v
  => NonEmpty v -> Cond ren sch tab
pin name = In @name

-- | Check that field value belongs to the list of values.
-- If the list is empty, the condition evaluates to @false@.
{-# INLINE pinArr #-}
pinArr :: forall name -> forall ren sch tab v. CDBValue sch tab (ApplyRenamer ren name) v
  => [v] -> Cond ren sch tab
pinArr name = InArr @name

-- | Conjunction.
(&&&) :: Cond ren sch tab -> Cond ren sch tab -> Cond ren sch tab
-- | Disjunction.
(|||) :: Cond ren sch tab -> Cond ren sch tab -> Cond ren sch tab
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
{-# INLINE (=??) #-}
{-# INLINE (~=?) #-}
{-# INLINE (~~?) #-}
-- | Compare field @fld@ with a value: @\<@, @\>@, @\<=@, @\>=@ and @=@.
--
-- The field name is given as a required type argument and the renamer is
-- applied to it. The value must have a compatible Haskell type ('CDBValue'):
--
-- > "name" =? ("John" :: Text)
-- > "created_at" >? someDay
--
-- @=@ with @NULL@ is never true: use '(=??)' or 'pnull' for nullable columns.
(<?),(>?),(<=?),(>=?),(=?)
   :: forall fld -> forall ren sch tab v. CDBValue sch tab (ApplyRenamer ren fld) v => v -> Cond ren sch tab
x <? b  = Cmp @x (:<)  b
x >? b  = Cmp @x (:>)  b
x <=? b = Cmp @x (:<=) b
x >=? b = Cmp @x (:>=) b
x =? b = Cmp @x (:=) b
-- | Null-safe equality (@IS NOT DISTINCT FROM@) for nullable columns only.
(=??)
  :: forall fld -> forall ren sch tab v
  . ( CDBFieldNullable sch tab (ApplyRenamer ren fld)
    , CDBValue sch tab (ApplyRenamer ren fld) v
    )
  => v -> Cond ren sch tab
x =?? b = Cmp @x NotDistinctFrom b
-- | Match field @fld@ with a pattern: @LIKE@ ('~=?') and case-insensitive
-- @ILIKE@ ('~~?').
--
-- > "name" ~~? ("%john%" :: Text)
(~=?),(~~?)
  :: forall fld -> forall ren sch tab v. CDBValue sch tab (ApplyRenamer ren fld) v => v -> Cond ren sch tab
x ~=? b  = Cmp @x Like b
x ~~? b  = Cmp @x ILike b
infix 4 <?, >?, <=?, >=?, =?, =??, ~=?, ~~?

-- | Direction of ordering. In both directions @NULL@ values go last
-- (@NULLS LAST@).
data OrdDirection = Asc | Desc deriving Show

-- | One item of @ORDER BY@ (or @DISTINCT ON@) for table @tab@.
--
-- Usually it is made with 'ascf', 'descf' or 'ordf'.
data OrdFld ren sch tab where
  OrdFld :: forall fld ren sch tab. CDBField sch tab (ApplyRenamer ren fld) =>
    OrdDirection -> OrdFld ren sch tab
  -- ^ Order by field @fld@ (given as a type application, the renamer is applied).
  UnsafeOrd :: CondMonad (Text, OrdDirection) -> OrdFld ren sch tab
  -- ^ Order by an SQL expression built in 'CondMonad' (see 'pUnsafeCond').
  -- The expression is not checked.

-- | @DISTINCT@ or @DISTINCT ON@ for table @tab@. Usually it is set with
-- 'qDistinct' or 'qDistinctOn'.
data Dist ren sch tab where
  Distinct :: Dist ren sch tab
  -- ^ @DISTINCT@: remove duplicate rows.
  -- | Having 'DistinctOn' we automatically add fields from 'DistinctOn'
  -- into the beginning of @ORDER BY@.
  -- (It is "good enough" and simpler than checking it on the type level.)
  --
  -- That's why we use t'OrdFld' which includes 'OrdDirection'.
  -- Naturally 'OrdDirection' is not used in the @DISTINCT ON@ part itself.
  --
  -- Besides that, the @DISTINCT ON@ part can include expressions like @ORDER BY@.
  -- We can also use 'UnsafeOrd' here.
  DistinctOn :: [OrdFld ren sch tab] -> Dist ren sch tab

{-# INLINE ordf #-}
-- | Sort by field in the given direction.
ordf
  :: forall fld -> forall sch tab. CDBField sch tab (ApplyRenamer ren fld)
  => OrdDirection -> OrdFld ren sch tab
ordf fld = OrdFld @fld

{-# INLINE ascf #-}
-- | Sort ascending by field.
ascf :: forall fld -> forall sch tab. CDBField sch tab (ApplyRenamer ren fld)
  => OrdFld ren sch tab
ascf fld = ordf fld Asc

{-# INLINE descf #-}
-- | Sort descending by field.
descf :: forall fld -> forall sch tab. CDBField sch tab (ApplyRenamer ren fld)
  => OrdFld ren sch tab
descf fld = ordf fld Desc

-- | @LIMIT@ and @OFFSET@. 'Nothing' means no limit / no offset.
data LO = LO
  { limit  :: Maybe Natural -- ^ @LIMIT@
  , offset :: Maybe Natural -- ^ @OFFSET@
  }
  deriving Show

-- | No limit and no offset.
defLO :: LO
defLO = LO Nothing Nothing

-- | Limit 1 and no offset.
lo1 :: LO
lo1 = LO (Just 1) Nothing
