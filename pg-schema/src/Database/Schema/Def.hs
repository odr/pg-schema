{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.Schema.Def where

import Data.Kind
import Data.List as L
import Data.List.Singletons as SP
import Data.Map as M
import Data.Ord.Singletons
import Data.Singletons.TH
import Data.Text as T
import PgSchema.Util
import Prelude.Singletons as SP
import Text.Show.Singletons
import Util.TH.LiftType


singletons [d|

  data NameNS' s = NameNS
    { nnsNamespace  :: s
    , nnsName       :: s }
    deriving (Show, Eq, Ord)
  data TypDef' s = TypDef
    { typCategory :: s
    , typElem     :: Maybe (NameNS' s)
    , typEnum     :: [s] }
    deriving (Show, Eq)

  data FldDef' s = FldDef
    { fdType        :: NameNS' s
    , fdNullable    :: Bool
    , fdHasDefault  :: Bool }
    deriving (Show, Eq)

  data TabDef' s = TabDef
    { tdFlds       :: [s]
    , tdKey        :: [s]
    , tdUniq       :: [[s]] }
    -- , tdFrom       :: [NameNS' s]
    -- , tdTo         :: [NameNS' s] }
    deriving (Show, Eq)

  data RelDef' s = RelDef
    { rdFrom    :: NameNS' s
    , rdTo      :: NameNS' s
    , rdCols    :: [(s,s)] }
    deriving (Show, Eq)

  data TabRel' s = TabRel
    { trFrom       :: [NameNS' s]
    , trTo         :: [NameNS' s] }
    deriving (Show, Eq)

  zip2With :: (a -> b -> c) -> [a] -> [[b]] -> [[c]]
  zip2With f = L.zipWith (L.map . f)

  map2 :: (a -> b) -> [a] -> [(a,b)]
  map2 f = L.map (\a -> (a, f a))

  map3 :: (b -> c) -> (a -> [b]) -> [a] -> [[(b,c)]]
  map3 f g = L.map (map2 f . g)

  data FldKind' s
    = FldPlain
    -- ^ simple field
    | FldTo (RelDef' s)
    -- ^ other records refer to this field (type is List)
    | FldFrom (RelDef' s)
    -- ^ field points to another record
    | FldUnknown s
    deriving (Show, Eq)
  |]

promote [d|
  getRelTab
    :: Eq s
    => [(NameNS' s, RelDef' s)] -> [(NameNS' s, RelDef' s)] -> s -> NameNS' s

  getRelTab froms tos s = find1 froms
    where
      find1 []         = find2 tos
      find1 ((a,b):xs) = if nnsName a == s then rdTo b else find1 xs
      find2 []         = error "No relation by name"
      find2 ((a,b):xs) = if nnsName a == s then rdFrom b else find2 xs

  getFldKind
    :: Eq s
    => TabDef' s -> [(NameNS' s, RelDef' s)] -> [(NameNS' s, RelDef' s)] -> s
    -> FldKind' s
  getFldKind (TabDef flds _ _) froms tos s = find1 flds
    where
      find1 []     = find2 froms
      find1 (x:xs) = if x == s then FldPlain else find1 xs
      find2 []         = find3 tos
      find2 ((a,b):xs) = if nnsName a == s then FldFrom b else find2 xs
      find3 []         = FldUnknown s
      find3 ((a,b):xs) = if nnsName a == s then FldTo b else find3 xs

  isAllMandatory' :: Eq s => (s -> FldDef' s) -> [s] -> [s] -> Bool
  isAllMandatory' f tabFlds recFlds =
    L.null $ L.filter (isMandatory . f) tabFlds L.\\ recFlds
    where
      isMandatory fd = not (fdNullable fd || fdHasDefault fd)

  hasNullable :: [FldDef' s] -> Bool
  hasNullable = L.any fdNullable

  |]

type NameNSK = NameNS' Symbol
type TypDefK = TypDef' Symbol
type FldDefK = FldDef' Symbol
type TabDefK = TabDef' Symbol
type RelDefK = RelDef' Symbol
type TabRelK = TabRel' Symbol
type FldKindK = FldKind' Symbol

type NameNS = NameNS' Text
type TypDef = TypDef' Text
type FldDef = FldDef' Text
type TabDef = TabDef' Text
type RelDef = RelDef' Text
type TabRel = TabRel' Text
type FldKind = FldKind' Text

infixr 9 ->>
(->>) :: Text -> Text -> NameNS
(->>) = NameNS

type ns ->> name = 'NameNS ns name

-- CTypDef
-- | instances will be generated by TH
class
  (ToStar name, ToStar (TTypDef sch name)) => CTypDef sch (name :: NameNSK) where

  type TTypDef sch name :: TypDefK

typDef :: forall sch name. CTypDef sch name => TypDef
typDef = demote @(TTypDef sch name)
genDefunSymbols [''TTypDef]

-- CFldDef
-- | instances will be generated by TH
class
  ( ToStar fname, ToStar tname
  , ToStar (TFldDef sch tname fname)
  , CTypDef sch (FdType (TFldDef sch tname fname)) )
  => CFldDef sch (tname::NameNSK) (fname::Symbol) where
  type TFldDef sch tname fname :: FldDefK

fldDef :: forall sch tname fname. CFldDef sch tname fname => FldDef
fldDef = demote @(TFldDef sch tname fname)

genDefunSymbols [''TFldDef]

-- CTabDef
-- | instances will be generated by TH
class
  ( ToStar name, ToStar (TTabDef sch name)
  , ToStar (SP.Map (TFldDefSym2 sch name) (TdFlds (TTabDef sch name)))
  , ToStar (SP.Map (TFldDefSym2 sch name) (TdKey (TTabDef sch name)))
  , ToStar
    (SP.Map (SP.MapSym1 (TFldDefSym2 sch name)) (TdUniq (TTabDef sch name)))
  , ToStar
    ( ConcatMap (MapSym1 (TFldDefSym2 sch name)) (TdUniq (TTabDef sch name)) )
  ) => CTabDef sch (name::NameNSK) where

  type TTabDef sch name :: TabDefK

type TFromTab sch name = RdFrom (TRelDef sch name)
type TFromFlds sch name = SP.Map FstSym0 (RdCols (TRelDef sch name))
type TToTab sch name = RdTo (TRelDef sch name)
type TToFlds sch name = SP.Map SndSym0 (RdCols (TRelDef sch name))

-- CRelDef
-- | instances will be generated by TH
class
  ( ToStar name, ToStar (TRelDef sch name)
  , CTabDef sch (TFromTab sch name)
  , CTabDef sch (TToTab sch name)
  , ToStar (SP.Map (TFldDefSym2 sch (TFromTab sch name)) (TFromFlds sch name))
  , ToStar (SP.Map (TFldDefSym2 sch (TToTab sch name)) (TToFlds sch name))
  )
  => CRelDef sch (name::NameNSK) where

  type TRelDef sch name :: RelDefK

genDefunSymbols [''TTabDef, ''TRelDef]
-- we can also defun CTabDef and CRelDef but this is not needed

type TTabRelFrom sch tab = Map2 (TRelDefSym1 sch) (TFrom sch tab)
type TTabRelTo sch tab = Map2 (TRelDefSym1 sch) (TTo sch tab)

class CTabRels sch (tab :: NameNSK) where
  type TFrom sch tab :: [NameNSK]
  type TTo sch tab :: [NameNSK]

genDefunSymbols [''TFrom, ''TTo]

type IsAllMandatory sch t rs =
  IsAllMandatory' (TFldDefSym2 sch t) (TdFlds (TTabDef sch t)) rs

type TFieldKind sch tab name =
  GetFldKind (TTabDef sch tab) (TTabRelFrom sch tab) (TTabRelTo sch tab) name

-- | instances will be generated by TH
class
  ( ToStar (TTabs sch) --, ToStar (TRels sch)
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
  -- type TRels sch    :: [NameNSK]
  type TTypes sch   :: [NameNSK]

-- type TRelDefs sch = Map2 (TRelDefSym1 sch) (TRels sch)
type TTabDefs sch = SP.Map (TTabDefSym1 sch) (TTabs sch)
type TTabFlds sch = SP.Map TdFldsSym0 (TTabDefs sch)
type TTabFldDefs sch =
  Zip2With (TFldDefSym1 sch) (TTabs sch) (TTabFlds sch)
type TTabRelFroms sch = Map3 (TRelDefSym1 sch) (TFromSym1 sch) (TTabs sch)
type TTabRelTos sch = Map3 (TRelDefSym1 sch) (TToSym1 sch) (TTabs sch)

--
data TabInfo = TabInfo
  { tiDef  :: TabDef
  , tiFlds :: M.Map Text FldDef
  , tiFrom :: M.Map NameNS RelDef
  , tiTo   :: M.Map NameNS RelDef }
  deriving (Show, Eq)

tabInfoMap :: forall sch. CSchema sch => M.Map NameNS TabInfo
tabInfoMap = M.fromList
  $ L.zip tabs
    $ L.zipWith4 TabInfo
      tabDefs
      (L.zipWith (\fs -> M.fromList . L.zip fs) tabFlds tabFldDefs)
      (M.fromList <$> tabRelFroms)
      (M.fromList <$> tabRelTos)
  where
    tabs = demote @(TTabs sch)
    tabDefs = demote @(TTabDefs sch)
    tabFlds = demote @(TTabFlds sch)
    tabFldDefs = demote @(TTabFldDefs sch)
    tabRelFroms = demote @(TTabRelFroms sch)
    tabRelTos = demote @(TTabRelTos sch)

typDefMap :: forall sch. CSchema sch => M.Map NameNS TypDef
typDefMap = M.fromList $ L.zip
  (demote @(TTypes sch))
  (demote @(SP.Map (TTypDefSym1 sch) (TTypes sch)))

type TRelTab sch t name = GetRelTab
  (Map2 (TRelDefSym1 sch) (TFrom sch t)) (Map2 (TRelDefSym1 sch) (TTo sch t))
  name

type family TabOnPath sch (t :: NameNSK) (path :: [Symbol]) :: NameNSK where
  TabOnPath sch t '[] = t
  TabOnPath sch t (x ': xs) = TabOnPath sch (TRelTab sch t x) xs
--
type family TabPath sch (t :: NameNSK) (path :: [Symbol]) :: Constraint where
  TabPath sch t '[] = ()
  TabPath sch t (x ': xs) = TabPath sch (TRelTab sch t x) xs
--
instance LiftType NameNS where
  liftType NameNS{..} =
    [t| $(liftType nnsNamespace) ->> $(liftType nnsName) |]

instance LiftType TypDef where
  liftType TypDef{..} = [t| 'TypDef
    $(liftType typCategory) $(liftType typElem) $(liftType typEnum) |]

instance LiftType FldDef where
  liftType FldDef{..} = [t| 'FldDef
    $(liftType fdType) $(liftType fdNullable) $(liftType fdHasDefault) |]

instance LiftType TabDef where
  liftType TabDef{..} =
    [t| 'TabDef $(liftType tdFlds) $(liftType tdKey) $(liftType tdUniq) |]

instance LiftType RelDef where
  liftType RelDef{..} =
    [t| 'RelDef $(liftType rdFrom) $(liftType rdTo) $(liftType rdCols) |]
--
