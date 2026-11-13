{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
module PgSchema.Schema where

import Data.List as L
import Data.Map as M
import Data.Singletons
import Data.Singletons.TH
import Data.String
import Data.Text as T hiding (show)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits as TL
import PgSchema.Utils.Instances()
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
  { typCategory :: s
  -- ^ PostgreSQL type category (@pg_type.typcategory@), e.g. @\"N\"@ (numeric),
  -- @\"S\"@ (string), @\"A\"@ (array), @\"E\"@ (enum).
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

-- | Direction of one path step relative to the current table.
data PathKind = FromHere | ToHere deriving (Show, Eq)

-- | One step of a @SELECT@ query path: logical branch name, DB fk name, direction.
data PathElem' s = PathElem
  { peName   :: s        -- ^ Relation name as given by the user (before the renamer).
  , peDbName :: s        -- ^ Name of the foreign-key constraint (after the renamer).
  , peKind   :: PathKind -- ^ Direction of the step.
  } deriving (Show, Eq)

-- | Field of a logical record: plain column, aggregate, or relation hop.
data RecField' s p
  = RFPlain (FldDef' s) -- ^ Ordinary column with 'FldDef''.
  | RFAggr (FldDef' s) AggrFun Bool
    -- ^ Aggregate field: 'FldDef'', which aggregate, and whether the result can
    -- be @NULL@ for a mandatory field: 'True' for 'PgSchema.DML.Aggr', 'False'
    -- for 'PgSchema.DML.Aggr'' (used with @GROUP BY@).
  | RFUnsafe [s] s
  -- ^ Plain field that we get as expression. E.g. @RFUnsafe [\"a\", \"b\"] \"? + ?\"@
  -- means @tn.a + tn.b@ in query.
  | RFToHere p [Ref' s]
    -- ^ Child relation: rows of table @p@ reference the current table
    -- (the foreign key goes from @p@ to here). The field is a list of child rows.
  | RFFromHere p [Ref' s]
    -- ^ Parent relation: the current table references table @p@
    -- (the foreign key goes from here to @p@). The field is one parent row
    -- ('Maybe' if the foreign key is nullable).
  | RFSelfRef p [Ref' s]
    -- ^ Self-referential relation (table @p@ references itself). It is used
    -- as 'RFToHere' for a list field and as 'RFFromHere' otherwise.
  deriving Show

-- | One column pair of a foreign key: source column, target column and their types.
data Ref' s = Ref
  { fromName :: s         -- ^ Source (child) column name.
  , fromDef :: FldDef' s  -- ^ Type/nullability of @fromName@.
  , toName :: s           -- ^ Target (parent) column name.
  , toDef :: FldDef' s    -- ^ Type/nullability of @toName@.
  } deriving Show

genSingletons
  [ ''AggrFun, ''NameNS', ''TypDef', ''FldDef', ''TabDef', ''RelDef', ''RelType
  , ''PathKind, ''PathElem'
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
type PathElemK = PathElem' Symbol
type PathElem = PathElem' Text

mkPathStep :: Text -> Text -> PathKind -> PathElem
mkPathStep name db kind = PathElem{peName = name, peDbName = db, peKind = kind}

-- | Broadcast step: 'peName' is the db/fk key from 'PgSchema.DML.qPath', not a Haskell field.
pathStepBroadcast :: PathElem -> Bool
pathStepBroadcast s = s.peName == s.peDbName

-- | Whether a query-path step applies to a runtime tree-path step.
pathStepMatches :: PathElem -> PathElem -> Bool
pathStepMatches cond rt =
  cond.peKind == rt.peKind
  && cond.peDbName == rt.peDbName
  && ( cond.peName == rt.peName
    || cond.peName == cond.peDbName
    )

pathsEqual :: [PathElem] -> [PathElem] -> Bool
pathsEqual xs ys =
  L.length xs == L.length ys
  && and (L.zipWith pathStepMatches xs ys)

pathIsPrefixOfBy :: (PathElem -> PathElem -> Bool) -> [PathElem] -> [PathElem] -> Bool
pathIsPrefixOfBy _ [] _ = True
pathIsPrefixOfBy f (x : xs) (y : ys) = f x y && pathIsPrefixOfBy f xs ys
pathIsPrefixOfBy _ _ [] = False

pathIsPrefixOf :: [PathElem] -> [PathElem] -> Bool
pathIsPrefixOf = pathIsPrefixOfBy $ flip pathStepMatches

parentPathMatches :: [PathElem] -> [PathElem] -> [PathElem] -> Bool
parentPathMatches p basePath piPath = pathsEqual p (basePath <> piPath)

infixr 9 ->>
(->>) :: Text -> Text -> NameNS
(->>) = NameNS

type ns ->> name = 'NameNS ns name

type SimpleType c = 'TypDef c 'Nothing '[]

type IsMandatory fd = Not (FdNullable fd || FdHasDefault fd)
type IsMandatory' sch tab fld = IsMandatory (GetFldDef sch tab fld)

type family RestMandatory' sch t (rs :: [Symbol]) (fs :: [Symbol]) (res :: [Symbol]) :: [Symbol] where
  RestMandatory' sch t rs '[] res = res
  RestMandatory' sch t rs (fld ': fs) res =
    RestMandatory'' sch t rs fs res fld (IsMandatory' sch t fld && Not (Elem' fld rs))

type family RestMandatory'' sch (t :: NameNSK) (rs :: [Symbol])
  (fs :: [Symbol]) (res :: [Symbol]) (fld :: Symbol) (b :: Bool) :: [Symbol] where
    RestMandatory'' sch t rs fs res fld 'True = fld ': RestMandatory' sch t rs fs res
    RestMandatory'' sch t rs fs res fld 'False = RestMandatory' sch t rs fs res

type RestMandatory sch t rs = RestMandatory' sch t rs (TdFlds (TTabDef sch t)) '[]

type family RestKey' (rs :: [Symbol]) (fs :: [Symbol]) (res :: [Symbol]) :: [Symbol] where
  RestKey' rs '[] res = res
  RestKey' rs (fld ': fs) res = RestKey'' rs fs fld res (Elem' fld rs)

type family RestKey'' rs fs fld (res :: [Symbol]) (b :: Bool) :: [Symbol] where
  RestKey'' rs fs fld res 'True = RestKey' rs fs res
  RestKey'' rs fs fld res 'False = RestKey' rs fs (fld ': res)

-- | Columns still missing from @rs@ to complete candidate key @keyCols@.
type RestKey rs keyCols = RestKey' rs keyCols '[]

type family AppendPK (pk :: [Symbol]) (uks :: [[Symbol]]) :: [[Symbol]] where
  AppendPK '[] uks = uks
  AppendPK pk uks = pk ': uks

-- | Keys accepted by 'PgSchema.Ann.CheckAllMandatoryOrHasKey' (tree upsert) and
-- 'PgSchema.Ann.CheckHasKey' (flat 'PgSchema.DML.updateByKey' /
-- 'PgSchema.DML.upsertByKey'): the primary key plus every unique constraint
-- from the schema (catalog order), including unique constraints over nullable
-- columns. A record \"has a key\" if it covers all columns of any of them.
--
-- At runtime the first covered key is used, in this order: the primary key,
-- unique constraints over @NOT NULL@ columns, other unique constraints.
type family IdentityCandidates sch tab :: [[Symbol]] where
  IdentityCandidates sch tab =
    AppendPK (TdKey (TTabDef sch tab)) (TdUniq (TTabDef sch tab))

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
-- | Definition of a database type. Instances are generated by schema generation.
class
  (ToStar name, ToStar (TTypDef sch name)) => CTypDef sch (name :: NameNSK) where

  type TTypDef sch name :: TypDefK

-- | Schema-level field kind for (sch, tab, field name).
-- Instances are generated by schema generation ("PgSchema.Generation") or
-- defined manually (e.g. "PgSchema.Schema.Catalog").
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
-- | Definition of a table. Instances are generated by schema generation.
class (ToStar name, ToStar (TTabDef sch name)) => CTabDef sch (name::NameNSK) where
  type TTabDef sch name :: TabDefK

-- | Relation definition for relation name @ref@.
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
-- Instances are generated by schema generation.
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

type family GetRelDef (rels :: [(NameNSK, RelDefK)]) (s :: Symbol) :: RelDefK where
  GetRelDef '[] s = TypeError ('Text "No relation by name" ':$$: 'ShowType s)
  GetRelDef ('(a, b) ': xs) s = If (NnsName a == s) b (GetRelDef xs s)

type family TRelFromTab sch t name :: NameNSK where
  TRelFromTab sch t name = RdTo (GetRelDef (Map2 (TRelDefSym1 sch) (TFrom sch t)) name)

type family TRelToTab sch t name :: NameNSK where
  TRelToTab sch t name = RdFrom (GetRelDef (Map2 (TRelDefSym1 sch) (TTo sch t)) name)

-- | Walk a directed @path@ from root table @t@.
--
-- Each step is a t'PathElem' (schema walk uses 'peDbName'). Result: end table and
-- its cardinality ('RelOne' for a single parent step, 'RelMany' for a single child).
type family TabOnDPath2 sch (t :: NameNSK) (path :: [PathElemK]) :: (NameNSK, RelType) where
  TabOnDPath2 sch t '[] = '(t, 'RelMany)
  TabOnDPath2 sch t '[ 'PathElem _ name 'FromHere] =
    '(TRelFromTab sch t name, 'RelOne)
  TabOnDPath2 sch t '[ 'PathElem _ name 'ToHere] =
    '(TRelToTab sch t name, 'RelMany)
  TabOnDPath2 sch t ('PathElem _ name 'FromHere ': xs) =
    TabOnDPath2 sch (TRelFromTab sch t name) xs
  TabOnDPath2 sch t ('PathElem _ name 'ToHere ': xs) =
    TabOnDPath2 sch (TRelToTab sch t name) xs

-- | Table reached after walking directed @path@ from @t@ ('Fst' of 'TabOnDPath2').
type TabAtPath sch t path = Fst (TabOnDPath2 sch t path)

-- | Current position has child cardinality ('RelMany'): root or child table.
type PathEndsMany sch t path = Snd (TabOnDPath2 sch t path) ~ 'RelMany

type RecField = RecField' Text
type Ref = Ref' Text

type family HasNullableRefs (rs :: [RefK]) :: Bool where
  HasNullableRefs '[] = 'False
  HasNullableRefs ('Ref _ ('FldDef _ 'True  _) _ _ ': rs) = 'True
  HasNullableRefs ('Ref _ ('FldDef _ 'False _) _ _ ': rs) = HasNullableRefs rs

-- | Value-level: whether any ref in the list has a nullable column.
-- Companion to type-level 'HasNullableRefs'.
hasNullableRefs :: [Ref] -> Bool
hasNullableRefs = L.any (fdNullable . fromDef)

qualName :: NameNS -> Text
qualName NameNS {..}
  | nnsNamespace == fromString "pg_catalog" = nnsName
  | otherwise = nnsNamespace <> fromString "." <> nnsName

type family HasRelName (rels :: [NameNSK]) (name :: Symbol) :: Bool where
  HasRelName '[] name = 'False
  HasRelName ('NameNS nns name ': rs) name = 'True
  HasRelName (_ ': rs) name = HasRelName rs name

type HasFromStep sch tab name = HasRelName (TFrom sch tab) name
type HasToStep   sch tab name = HasRelName (TTo sch tab) name

-- | Comma-separated names of relations (for error messages).
type family RelNamesMsg (rels :: [NameNSK]) :: ErrorMessage where
  RelNamesMsg '[] = TL.Text "<none>"
  RelNamesMsg '[ 'NameNS _ n ] = TL.Text n
  RelNamesMsg ('NameNS _ n ': rs) = TL.Text n :<>: TL.Text ", " :<>: RelNamesMsg rs

-- | Note for error messages when the renamer changed the name given by user.
type family RenamedMsg (orig :: Symbol) (name :: Symbol) :: ErrorMessage where
  RenamedMsg name name = TL.Text ""
  RenamedMsg orig name =
    TL.Text " (" :<>: ShowType orig :<>: TL.Text " after applying the renamer)"

-- | Resolve 'PathKind' for 'PgSchema.DML.qPath' / 'PgSchema.DML.qPathFromHere' /
-- 'PgSchema.DML.qPathToHere'.
--
-- @hasFrom@ / @hasTo@: whether @name@ is in 'TFrom' / 'TTo' of @tab@
-- (both are 'True' for a self-reference relation).
-- @orig@ is the name as given by user, @name@ is the name after applying the renamer.
type family ResolvePathKind
  (pathKind :: Maybe PathKind) (hasFrom :: Bool) (hasTo :: Bool) sch (tab :: NameNSK)
  (orig :: Symbol) (name :: Symbol)
  :: PathKind where
  ResolvePathKind _ 'False 'False sch tab orig name = TypeError
    ( TL.Text "In schema " :<>: ShowType sch
    :$$: TL.Text "for table " :<>: ShowType tab
    :$$: TL.Text "relation " :<>: ShowType name :<>: RenamedMsg orig name
      :<>: TL.Text " is not defined."
    :$$: TL.Text ""
    :$$: TL.Text "Valid values are:"
    :$$: TL.Text "  Foreign key constraints to parent tables (qPathFromHere or qPath): "
      :<>: RelNamesMsg (TFrom sch tab) :<>: TL.Text "."
    :$$: TL.Text "  Foreign key constraints from child tables (qPathToHere or qPath): "
      :<>: RelNamesMsg (TTo sch tab) :<>: TL.Text "."
    :$$: TL.Text "" )
  ResolvePathKind ('Just 'FromHere) 'True _ _ _ _ _ = 'FromHere
  ResolvePathKind ('Just 'ToHere) _ 'True _ _ _ _ = 'ToHere
  ResolvePathKind 'Nothing 'True 'False _ _ _ _ = 'FromHere
  ResolvePathKind 'Nothing 'False 'True _ _ _ _ = 'ToHere
  ResolvePathKind ('Just 'FromHere) 'False 'True _ tab _ name = TypeError
    ( TL.Text "Relation is not available in from-here direction."
    :$$: TL.Text "Use qPathToHere or qPath."
    :$$: TL.Text ""
    :$$: TL.Text "Table: " :<>: ShowType tab
    :$$: TL.Text "Relation: " :<>: ShowType name
    :$$: TL.Text "" )
  ResolvePathKind ('Just 'ToHere) 'True 'False _ tab _ name = TypeError
    ( TL.Text "Relation is not available in to-here direction."
    :$$: TL.Text "Use qPathFromHere or qPath."
    :$$: TL.Text ""
    :$$: TL.Text "Table: " :<>: ShowType tab
    :$$: TL.Text "Relation: " :<>: ShowType name
    :$$: TL.Text "" )
  ResolvePathKind 'Nothing 'True 'True _ tab _ name = TypeError
    ( TL.Text "qPath cannot be used for self-reference relation."
    :$$: TL.Text "Use qPathFromHere or qPathToHere."
    :$$: TL.Text ""
    :$$: TL.Text "Table: " :<>: ShowType tab
    :$$: TL.Text "Relation: " :<>: ShowType name
    :$$: TL.Text "" )
