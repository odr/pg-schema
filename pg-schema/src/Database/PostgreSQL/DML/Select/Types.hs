{-# LANGUAGE ImportQualifiedPost #-}
module Database.PostgreSQL.DML.Select.Types where

import Control.Monad.RWS
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Kind
import Data.List.NonEmpty as NE
import Data.String
import Data.Text as T
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


data QueryParam sch t = QueryParam
  { qpConds :: ![CondWithPath sch t]
  , qpOrds  :: ![OrdWithPath sch t]
  , qpLOs   :: ![LimOffWithPath sch t] }

qpEmpty :: forall sch t. QueryParam sch t
qpEmpty = QueryParam [] [] []

data CondWithPath sch t where
  CondWithPath ::  forall (path :: [Symbol]) sch t. ToStar path
    => Cond sch (TabOnPath sch t path) -> CondWithPath sch t

data OrdWithPath sch t where
  OrdWithPath :: forall (path :: [Symbol]) sch t. ToStar path
    => [OrdFld sch (TabOnPath sch t path)] -> OrdWithPath sch t

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

{- | Monad to generate condition.
Read: Stack of numbers of parent tables. The last one is "current table"
Write: List of placeholder-values.
  Note: We have to generate sql from top to bottom to correct order of fields
State: Maximal number of tables "in use"
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
    , CanConvertPG sch (FdType (TFldDef sch tab fld))
      (FdNullable (TFldDef sch tab fld)) v)
    => Cmp -> v -> Cond sch tab
  In :: forall fld v sch tab.
    ( CFldDef sch tab fld, ToField v, Show v
    , CanConvertPG sch (FdType (TFldDef sch tab fld))
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

pnull :: forall sch tab. forall name ->
  (CFldDef sch tab name, FdNullable (TFldDef sch tab name) ~ 'True) =>
  Cond sch tab
pnull name = Null @name

pchild :: forall sch. forall name ->
  (CTabDef sch (RdFrom (TRelDef sch name)), CRelDef sch name) =>
  TabParam sch (RdFrom (TRelDef sch name)) ->
  Cond sch (RdFrom (TRelDef sch name)) -> Cond sch (RdTo (TRelDef sch name))
pchild name = Child @name

pparent :: forall sch. forall ref ->
  ( CTabDef sch (RdTo (TRelDef sch ref)) , CRelDef sch ref ) =>
  Cond sch (RdTo (TRelDef sch ref)) -> Cond sch (RdFrom (TRelDef sch ref))
pparent name = Parent @name @sch

pnot :: Cond sch tab -> Cond sch tab
pnot = Not

pUnsafeCond :: CondMonad Text -> Cond sch tab
pUnsafeCond = UnsafeCond

pin :: forall name -> forall sch tab v.
  ( CFldDef sch tab name, Show v, ToField v
  , CanConvertPG sch (FdType (TFldDef sch tab name))
    (FdNullable (TFldDef sch tab name)) v ) =>
  NonEmpty v -> Cond sch tab
pin name = In @name

(&&&), (|||) :: Cond sch tab -> Cond sch tab -> Cond sch tab
(&&&) = BoolOp And
(|||) = BoolOp Or
infixl 2 |||
infixl 3 &&&
--
(<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?) :: forall fld -> forall sch tab v.
  ( CFldDef sch tab fld, ToField v, Show v
  , CanConvertPG sch (FdType (TFldDef sch tab fld))
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

-- type RelTab2 sch rel tab = If (RdTo (TRelDef sch rel) == tab)
--   (RdFrom (TRelDef sch rel))
--   (If (RdFrom (TRelDef sch rel) == tab)
--     (RdTo (TRelDef sch rel))
--     (TypeError (TE.Text "Relation " :<>: TE.ShowType rel
--       :<>: TE.Text " is not connected to table " :<>: TE.ShowType tab)))

data OrdFld sch tab where
  OrdFld :: forall fld sch tab. CFldDef sch tab fld =>
    OrdDirection -> OrdFld sch tab
  -- SelFld :: forall (rel :: NameNSK) (fld :: Symbol) sch tab. CRelDef sch rel =>
  --   Cond sch (RelTab2 sch rel tab) -> [OrdFld sch (RelTab2 sch rel tab)] ->
  --   OrdDirection -> OrdFld sch tab
  UnsafeOrd :: CondMonad Text -> OrdFld sch tab

ordf
  :: forall fld
  -> forall sch tab. CFldDef sch tab fld
  => OrdDirection -> OrdFld sch tab
ordf fld = OrdFld @fld

ascf :: forall fld -> forall sch tab. CFldDef sch tab fld => OrdFld sch tab
ascf fld = ordf fld Asc

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

data LimOffWithPath sch t where
  LimOffWithPath :: forall (path :: [Symbol]) sch t. (TabPath sch t path, ToStar path)
    => LO -> LimOffWithPath sch t
