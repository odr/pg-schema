{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ParallelListComp #-}
module PgSchema.Schema where

import Data.Kind
import Data.List as L
import Data.Map as M
import Data.Singletons
import Data.Singletons.TH
import Data.String
import Data.Text as T hiding (show)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits
-- import PgSchema.Utils.Instances()
-- import Data.Maybe.Singletons()
import Data.Bool.Singletons()
import Data.Maybe.Singletons()
import PgSchema.Utils.Internal
import PgSchema.Utils.TF


-- | Supported SQL aggregate functions
data AggrFun = ACount | AMin | AMax | ASum | AAvg
  deriving Show

-- | Qualified PostgreSQL name: namespace (schema) + local name.
data NameNS' s = NameNS
  { nnsNamespace :: s -- ^ Namespace (database schema), e.g. @public@ or @pg_catalog@.
  , nnsName :: s      -- ^ Unqualified table / relation / type name.
  } deriving (Show, Eq, Ord)

-- | Description of a PostgreSQL type (category, array element, enum labels).
data TypDef' s = TypDef
  { typCategory :: s  -- ^ PostgreSql internal type category
  , typElem :: Maybe (NameNS' s) -- ^ For array types: element type name; 'Nothing' for non-arrays.
  , typEnum :: [s] -- ^ Enum label names for enum types; empty list for non-enums.
  } deriving Show

-- | Column-level type and nullability/default flags.
data FldDef' s = FldDef
  { fdType :: NameNS' s   -- ^ PostgreSQL type of the column.
  , fdNullable :: Bool    -- ^ 'True' if the column allows NULL.
  , fdHasDefault :: Bool -- ^ 'True' if the column has a default value; affects mandatory-field logic.
  } deriving Show

-- | Table shape: ordered column names, primary key, unique constraints.
data TabDef' s = TabDef
  { tdFlds :: [s]     -- ^ Physical column names in order.
  , tdKey :: [s]      -- ^ Primary key column names.
  , tdUniq :: [[s]]   -- ^ Unique constraints as lists of column names.
  } deriving Show

-- | Foreign-key-style link between two qualified tables and column mapping.
data RelDef' s = RelDef
  { rdFrom :: NameNS' s -- ^ Referencing table (child).
  , rdTo :: NameNS' s   -- ^ Referenced table (parent).
  , rdCols :: [(s, s)]  -- ^ Pairs @(fromColumn, toColumn)@.
  } deriving Show

-- | Cardinality of a relation edge (one vs many from this table’s perspective).
data RelType = RelOne | RelMany deriving Show

-- | Field of a logical record: plain column, aggregate, or relation hop.
data RecField' s p
  = RFEmpty s -- ^ Placeholder / unnamed slot (depending on schema codegen).
  | RFPlain (FldDef' s) -- ^ Ordinary column with 'FldDef''.
  | RFAggr (FldDef' s) AggrFun Bool
    -- ^ Aggregate field: 'FldDef'', which aggregate, and whether it is allowed outside @GROUP BY@
    -- (when 'True': any select; when 'False': only with @GROUP BY@).
  | RFToHere p [Ref' s]
    -- ^ Relation: navigate @p@ toward the current table (“to here”).
  | RFFromHere p [Ref' s]
    -- ^ Relation: navigate @p@ away from the current table (“from here”).
  | RFSelfRef p [Ref' s] -- ^ Self-referential relation through path @p@.
  deriving Show

-- | One step of a join path: source column, types, target column.
data Ref' s = Ref
  { fromName :: s         -- ^ Source (child) column name.
  , fromDef :: FldDef' s  -- ^ Type/nullability of @fromName@.
  , toName :: s           -- ^ Target (parent) column name.
  , toDef :: FldDef' s    -- ^ Type/nullability of @toName@.
  } deriving Show

genSingletons
  [ ''AggrFun, ''NameNS', ''TypDef', ''FldDef', ''TabDef', ''RelDef', ''RelType
  , ''RecField', ''Ref' ]

type NameNSK = NameNS' Symbol
type TypDefK = TypDef' Symbol
type FldDefK = FldDef' Symbol
type TabDefK = TabDef' Symbol
type RelDefK = RelDef' Symbol
type RecFieldK = RecField' Symbol
type RefK = Ref' Symbol

type NameNS = NameNS' Text
type TypDef = TypDef' Text
type FldDef = FldDef' Text
type TabDef = TabDef' Text
type RelDef = RelDef' Text

infixr 9 ->>
(->>) :: Text -> Text -> NameNS
(->>) = NameNS

type ns ->> name = 'NameNS ns name

type SimpleType c = 'TypDef c 'Nothing '[]

type family GetRelTab (froms :: [(NameNSK, RelDefK)])
  (tos :: [(NameNSK, RelDefK)]) (s :: Symbol) :: (NameNSK, RelType) where
    GetRelTab '[] '[] s = TypeError ('Text "No relation by name" ':$$: 'ShowType s)
    GetRelTab ('(a,b) ':xs) ys s =
      If (NnsName a == s) '(RdTo b, RelOne) (GetRelTab xs ys s)
    GetRelTab '[] ('(c,d) ':ys) s =
      If (NnsName c == s) '(RdFrom d, RelMany) (GetRelTab '[] ys s)

type IsMandatory fd = Not (FdNullable fd || FdHasDefault fd)
type IsMandatory' sch tab fld = IsMandatory (GetFldDef sch tab fld)

type family RestMandatory' sch t (rs :: [Symbol]) (fs :: [Symbol]) (res :: [Symbol]) :: [Symbol] where
  RestMandatory' sch t rs '[] res = res
  RestMandatory' sch t rs (fld ': fs) res = RestMandatory' sch t rs fs
    (If (IsMandatory' sch t fld && Not (Elem' fld rs)) (fld ': res) res)

type RestMandatory sch t rs = RestMandatory' sch t rs (TdFlds (TTabDef sch t)) '[]

type family RestPK' sch t (rs :: [Symbol]) (fs :: [Symbol]) (res :: [Symbol]) :: [Symbol] where
  RestPK' sch t rs '[] res = res
  RestPK' sch t rs (fld ': fs) res =
    RestPK' sch t rs fs (If (Not (Elem' fld rs)) (fld ': res) res)

type RestPK sch t rs = RestPK' sch t rs (TdKey (TTabDef sch t)) '[]

simpleType :: Text -> TypDef
simpleType c = TypDef c Nothing []

type SymNat = (Symbol, Nat)

type KnownSymNat sn = (SingI (NameSymNat sn))

nameSymNat :: forall sn -> KnownSymNat sn => Text
nameSymNat sn = demote @(NameSymNat sn)
-- >>> nameSymNat ("test", 42)
-- "test___42"

type family NameSymNat (sn :: SymNat) where
  NameSymNat '(s,0) = s
  NameSymNat '(s,n) = AppendSymbol s (AppendSymbol "___" (NatToSymbol n))

-- CTypDef
-- | instances will be generated by code generation
class
  (ToStar name, ToStar (TTypDef sch name)) => CTypDef sch (name :: NameNSK) where

  type TTypDef sch name :: TypDefK

-- | Schema-level field kind for (sch, tab, field name).
-- Instances are generated by codegen (Gen) or defined manually (e.g. Catalog).
class (ToStar (TDBFieldInfo sch tab name), ToStar tab, ToStar name)
  => CDBFieldInfo sch (tab :: NameNSK) (name :: Symbol) where
    type TDBFieldInfo sch tab name :: RecFieldK NameNSK

-- | Extract 'FldDefK' from a plain field kind (for conditions, order, etc.).
type family PlainFldDef (r :: RecFieldK NameNSK) :: FldDefK where
  PlainFldDef ('RFPlain fd) = fd

-- | Field definition for (sch, tab, name) when the field is plain. Used by Select/Update conditions.
type family GetFldDef (sch :: k) (tab :: NameNSK) (name :: Symbol) :: FldDefK where
  GetFldDef sch tab name = PlainFldDef (TDBFieldInfo sch tab name)

-- CTabDef
-- | instances will be generated by code generation
class (ToStar name, ToStar (TTabDef sch name)) => CTabDef sch (name::NameNSK) where
  type TTabDef sch name :: TabDefK

-- | Relation definition for relation name ref.
class
  ( ToStar (TRelDef sch ref)
  , CTabDef sch (RdFrom (TRelDef sch ref))
  , CTabDef sch (RdTo (TRelDef sch ref)) )
  => CRelDef sch (ref :: NameNSK) where
    type TRelDef sch (ref :: NameNSK) :: RelDefK

getFldDef :: forall sch t n. ToStar (TDBFieldInfo sch t n) => FldDef
getFldDef = case demote @(TDBFieldInfo sch t n) of
  RFPlain fd -> fd
  _ -> error "impossible"

class CTabRels sch (tab :: NameNSK) where
  type TFrom sch tab :: [NameNSK]
  type TTo sch tab :: [NameNSK]

genDefunSymbols [''TTypDef, ''TDBFieldInfo, ''GetFldDef, ''TTabDef, ''TRelDef, ''TFrom, ''TTo]

type family Map2 (f :: a ~> b) (xs :: [a]) :: [(a,b)] where
  Map2 f '[] = '[]
  Map2 f (x ': xs) = '(x, Apply f x) ': Map2 f xs

type family Map3 (f :: a ~> b) (g :: c ~> [a]) (xs :: [c]) :: [[(a,b)]] where
  Map3 f g '[] = '[]
  Map3 f g (x ': xs) = Map2 f (Apply g x) ': Map3 f g xs

type TTabRelFrom sch tab = Map2 (TRelDefSym1 sch) (TFrom sch tab)
type TTabRelTo sch tab = Map2 (TRelDefSym1 sch) (TTo sch tab)

-- | The main class for schema. All DML operations are based on this class.
-- It contains all the information about the schema: tables, relations, fields, types.
--
-- This class guarantees that we can demote all the necessary information about the schema from type level to value level.
--
-- Instances will be generated by code generation
class
  ( ToStar (TTabs sch)
  , ToStar (TTabRelFroms sch)
  , ToStar (TTabRelTos sch)
  , ToStar (TTabFldDefs sch)
  , ToStar (TTabFlds sch)
  , ToStar (TTabDefs sch)
  , ToStar (TTypes sch)
  , ToStar (Map1 (TTypDefSym1 sch) (TTypes sch))
  )
  => CSchema sch where

  type TTabs sch    :: [NameNSK]
  type TTypes sch   :: [NameNSK]

type TTabDefs sch = Map1 (TTabDefSym1 sch) (TTabs sch)
type TTabFlds sch = Map1 TdFldsSym0 (TTabDefs sch)

type family TTabFldDefsList sch (tabs :: [NameNSK]) (tabFlds :: [[Symbol]]) :: [[FldDefK]] where
  TTabFldDefsList sch '[] '[] = '[]
  TTabFldDefsList sch (t ': ts) (f ': fs) = Map1 (GetFldDefSym2 sch t) f ': TTabFldDefsList sch ts fs

type TTabFldDefs sch = TTabFldDefsList sch (TTabs sch) (TTabFlds sch)
type TTabRelFroms sch = Map3 (TRelDefSym1 sch) (TFromSym1 sch) (TTabs sch)
type TTabRelTos sch = Map3 (TRelDefSym1 sch) (TToSym1 sch) (TTabs sch)

--
data TabInfo = TabInfo
  { tiDef  :: TabDef
  , tiFlds :: M.Map Text FldDef
  , tiFrom :: M.Map NameNS RelDef
  , tiTo   :: M.Map NameNS RelDef }
  deriving Show

tabInfoMap :: forall sch. CSchema sch => M.Map NameNS TabInfo
tabInfoMap = M.fromList
  [ (tabName, tabInfo)
  | tabName <- demote @(TTabs sch)
  | tabInfo <-
    [ TabInfo{..}
    | tiDef <- demote @(TTabDefs sch)
    | tiFlds <-
      [ M.fromList $ L.zip fs ds
      | fs <- demote @(TTabFlds sch)
      | ds <- demote @(TTabFldDefs sch) ]
    | tiFrom <- M.fromList <$> demote @(TTabRelFroms sch)
    | tiTo <- M.fromList <$> demote @(TTabRelTos sch) ] ]

typDefMap :: forall sch. CSchema sch => M.Map NameNS TypDef
typDefMap = M.fromList $ L.zip
  (demote @(TTypes sch)) (demote @(Map1 (TTypDefSym1 sch) (TTypes sch)))

type TRelTab sch t name = GetRelTab
  (Map2 (TRelDefSym1 sch) (TFrom sch t)) (Map2 (TRelDefSym1 sch) (TTo sch t))
  name

type family TabOnPath2 sch (t :: NameNSK) (path :: [Symbol]) :: (NameNSK, RelType) where
  TabOnPath2 sch t '[] = '(t, 'RelMany)
  TabOnPath2 sch t '[x] = TRelTab sch t x
  TabOnPath2 sch t (x ': xs) = TabOnPath2 sch (Fst (TRelTab sch t x)) xs

type TabOnPath sch (t :: NameNSK) (path :: [Symbol]) = Fst (TabOnPath2 sch t path)

--
type family TabPath sch (t :: NameNSK) (path :: [Symbol]) :: Constraint where
  TabPath sch t '[] = ()
  TabPath sch t (x ': xs) = TabPath sch (Fst (TRelTab sch t x)) xs

type RecField = RecField' Text
type Ref = Ref' Text

-- | Value-level: whether any ref in the list has a nullable column.
-- Companion to type-level 'HasNullableRefs'.
hasNullableRefs :: [Ref] -> Bool
hasNullableRefs = L.any (fdNullable . fromDef)

qualName :: NameNS -> Text
qualName NameNS {..}
  | nnsNamespace == fromString "pg_catalog" = nnsName
  | otherwise = nnsNamespace <> fromString "." <> nnsName
