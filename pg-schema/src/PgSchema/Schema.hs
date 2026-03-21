{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ParallelListComp #-}
module PgSchema.Schema where

import Data.Kind
import Data.List as L
import Data.List.Singletons as SP
import Data.Map as M
import Data.Singletons.TH
import Data.String
import Data.Text as T hiding (show)
import GHC.TypeLits
import PgSchema.Utils.Internal
import Prelude.Singletons as SP


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

promote [d|
  getRelTab
    :: Eq s
    => [(NameNS' s, RelDef' s)] -> [(NameNS' s, RelDef' s)] -> s
    -> (NameNS' s, RelType)
  getRelTab froms tos s = find1 froms
    where
      find1 []         = find2 tos
      find1 ((a,b):xs) = if nnsName a == s then (rdTo b, RelOne) else find1 xs
      find2 []         = error "No relation by name"
      find2 ((a,b):xs) = if nnsName a == s then (rdFrom b, RelMany) else find2 xs

  restMandatory' :: Eq s => (s -> FldDef' s) -> TabDef' s -> [s] -> [s]
  restMandatory' f td recFlds =
    L.filter (isMandatory . f) (tdFlds td) L.\\ recFlds
    where
      isMandatory fd = not (fdNullable fd || fdHasDefault fd)

  restPK' :: Eq s => TabDef' s -> [s] -> [s]
  restPK' td recFlds = tdKey td L.\\ recFlds
  |]


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

simpleType :: Text -> TypDef
simpleType c = TypDef c Nothing []

type SymNat = (Symbol, Nat)

type KnownSymNat sn = (SingI (NameSymNat sn))

nameSymNat :: forall sn -> KnownSymNat sn => Text
nameSymNat sn = demote @(NameSymNat sn)
-- >>> nameSymNat ("test", 42)
-- "test___42"

type NameSymNat (sn :: SymNat) = If (Snd sn == 0) (Fst sn)
  (AppendSymbol (Fst sn) (AppendSymbol "___" (Show_ (Snd sn))))

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

type RestMandatory sch t rs =
  RestMandatory' (GetFldDefSym2 sch t) (TTabDef sch t) rs

type RestPK sch t rs = RestPK' (TTabDef sch t) rs

promote [d|
  map2 :: (a -> b) -> [a] -> [(a,b)]
  map2 f = L.map (\a -> (a, f a))

  map3 :: (b -> c) -> (a -> [b]) -> [a] -> [[(b,c)]]
  map3 f g = L.map (map2 f . g)
  |]

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
  , ToStar (SP.Map (TTypDefSym1 sch) (TTypes sch))
  )
  => CSchema sch where

  type TTabs sch    :: [NameNSK]
  type TTypes sch   :: [NameNSK]

type TTabDefs sch = SP.Map (TTabDefSym1 sch) (TTabs sch)
type TTabFlds sch = SP.Map TdFldsSym0 (TTabDefs sch)

type family TTabFldDefsList sch (tabs :: [NameNSK]) (tabFlds :: [[Symbol]]) :: [[FldDefK]] where
  TTabFldDefsList sch '[] '[] = '[]
  TTabFldDefsList sch (t ': ts) (f ': fs) = SP.Map (GetFldDefSym2 sch t) f ': TTabFldDefsList sch ts fs

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
    | tiTo <- M.fromList <$> demote @(TTabRelTos sch)
    ]
  ]

typDefMap :: forall sch. CSchema sch => M.Map NameNS TypDef
typDefMap = M.fromList $ L.zip
  (demote @(TTypes sch)) (demote @(SP.Map (TTypDefSym1 sch) (TTypes sch)))

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
