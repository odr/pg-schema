{-# LANGUAGE ImportQualifiedPost #-}
module Database.PostgreSQL.DML.Select.Types where

import Control.Monad.RWS
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Kind
import Data.List.NonEmpty as NE
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

type MonadQuery sch t m =
  (CSchema sch, MonadRWS (QueryRead sch t) [SomeToField] QueryState m)

data Cmp = (:=) | (:<=) | (:>=) | (:>) | (:<) | Like | ILike
  deriving (Show, Eq, Generic)

instance FromJSON Cmp
instance ToJSON Cmp

data BoolOp = And | Or
  deriving (Show, Eq, Generic)

instance FromJSON BoolOp
instance ToJSON BoolOp

showCmp :: Cmp -> Text
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
    (CFldDef sch tab fld, FdNullable (TFldDef sch tab fld) ~ 'True)
    => Cond sch tab
  Not :: Cond sch tab -> Cond sch tab
  BoolOp :: BoolOp -> Cond sch tab -> Cond sch tab -> Cond sch tab
  -- condition "EXIST"
  Child :: forall ref rel sch tab from.
    ( rel ~ TRelDef sch ref, tab ~ RdTo rel, from ~ RdFrom rel
    , CTabDef sch from, CRelDef sch ref )
    => TabParam sch from -> Cond sch from -> Cond sch tab
  -- condition "JOIN"
  Parent :: forall ref rel sch tab to.
    ( rel ~ TRelDef sch ref, tab ~ RdFrom rel, CTabDef sch to, to ~ RdTo rel
    , CRelDef sch ref )
    => Cond sch to -> Cond sch tab
  UnsafeCond :: CondMonad Text -> Cond sch tab

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

--
pcmp
  :: forall name sch tab v .
    ( CFldDef sch tab name, Show v, ToField v
    , CanConvertPG sch (FdType (TFldDef sch tab name))
      (FdNullable (TFldDef sch tab name)) v )
  => Cmp -> v -> Cond sch tab
pcmp = Cmp @name

pnull
  :: forall name sch tab .
    (CFldDef sch tab name, FdNullable (TFldDef sch tab name) ~ 'True)
  => Cond sch tab
pnull = Null @name

pchild
  :: forall name sch tab rel .
    ( rel ~ TRelDef sch name, tab ~ RdTo rel
    , CTabDef sch (RdFrom rel), CRelDef sch name )
  => TabParam sch (RdFrom rel) -> Cond sch (RdFrom rel) -> Cond sch tab
pchild = Child @name

pparent
  :: forall name sch rel.
    ( rel ~ TRelDef sch name, CTabDef sch (RdTo rel), CRelDef sch name )
  => Cond sch (RdTo rel) -> Cond sch (RdFrom rel)
pparent = Parent @name

-- -- --
-- instance
--   ( CFldDef sch tab fld, Show v, ToField v
--   , CanConvertPG sch (FdType (TFldDef sch tab fld))
--     (FdNullable (TFldDef sch tab fld)) v )
--   => IsLabel fld (Cmp -> v -> Cond sch tab) where
--   fromLabel = Cmp @_ @_ @fld Proxy
--
pnot :: Cond sch tab -> Cond sch tab
pnot = Not

pUnsafeCond :: CondMonad Text -> Cond sch tab
pUnsafeCond = UnsafeCond

pin
  :: forall name sch tab v .
    ( CFldDef sch tab name, Show v, ToField v
    , CanConvertPG sch (FdType (TFldDef sch tab name))
      (FdNullable (TFldDef sch tab name)) v )
  => NonEmpty v -> Cond sch tab
pin = In @name

(&&&), (|||) :: Cond sch tab -> Cond sch tab -> Cond sch tab
(&&&) = BoolOp And
(|||) = BoolOp Or
infixl 2 |||
infixl 3 &&&
--
(<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?)
  :: forall sch tab v. (Cmp -> v -> Cond sch tab) -> v -> Cond sch tab
x <? b  = x (:<)  b
x >? b  = x (:>)  b
x <=? b = x (:<=) b
x >=? b = x (:>=) b
x =? b = x (:=) b
x ~=? b  = x Like b
x ~~? b  = x ILike b
infix 4 <?, >?, <=?, >=?, =?, ~=?, ~~?

data OrdDirection = Asc | Desc deriving Show

data OrdFld sch tab where
  OrdFld :: forall fld sch tab. CFldDef sch tab fld
    => OrdDirection -> OrdFld sch tab
  SelFld :: forall rel fld t sch tab rd.
    ( CSchema sch, CFldDef sch t fld, CRelDef sch rel, rd ~ TRelDef sch rel
    , If (RdTo rd == tab) (RdFrom rd ~ t)
      (If (RdFrom rd == tab) (RdTo rd ~ t)
        (TypeError (TE.Text "Relation " :<>: TE.ShowType rel
          :<>: TE.Text " is not connected to table " :<>: TE.ShowType tab))) )
    => Cond sch t -> [OrdFld sch t] -> OrdDirection -> OrdFld sch tab
  UnsafeOrd :: CondMonad Text -> OrdFld sch tab

ordf :: forall fld sch tab. CFldDef sch tab fld => OrdDirection -> OrdFld sch tab
ordf = OrdFld @fld

ascf :: forall fld sch tab. CFldDef sch tab fld => OrdFld sch tab
ascf = ordf @fld Asc

descf :: forall fld sch tab. CFldDef sch tab fld => OrdFld sch tab
descf = ordf @fld Desc

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
