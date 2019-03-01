{-# LANGUAGE CPP                     #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.Schema.Def where

import Data.Kind
import Data.List as L
import Data.Map as M
import Data.Singletons.Prelude as SP
import Data.Singletons.Prelude.List as SP
import Data.Singletons.TH
import Data.Text as T
import Util.ToStar


singletons [d|

  data TypDef' s = TypDef
    { typCategory :: s
    , typElem     :: Maybe s
    , typEnum     :: [s] }
    deriving Show

  data FldDef' s = FldDef
    { fdType        :: s
    , fdNullable    :: Bool
    , fdHasDefault  :: Bool }
    deriving Show

  data TabDef' s = TabDef
    { tdFlds       :: [s]
    , tdKey        :: [s]
    , tdUniq       :: [[s]] }
    deriving Show

  data RelDef' s = RelDef
    { rdFrom    :: s
    , rdTo      :: s
    , rdCols    :: [(s,s)] }
    deriving Show

  zip2With :: (a -> b -> c) -> [a] -> [[b]] -> [[c]]
  zip2With f as = L.zipWith (\a -> L.map (f a)) as

  map2 :: (a -> b) -> [a] -> [(a,b)]
  map2 f as = L.map (\a -> (a, f a)) as

  data FldKind
    = FldPlain -- ^ simple field
    | FldTo    -- ^ other records referenced to this field (type is List)
    | FldFrom  -- ^ field point to another record
    | FldUnknown
    deriving (Show, Read, Eq)

  |]

promote [d|
  getFldKind :: Eq s => TabDef' s -> [(s, RelDef' s)] -> s -> s -> FldKind
  getFldKind (TabDef flds _ _) rs tab s = case L.find (== s) flds of
    Just _ -> FldPlain
    _ -> case L.lookup s rs of
      Just (RelDef rfrom rto _) | rfrom == tab  -> FldFrom
                                | rto == tab    -> FldTo
      _                                         -> FldUnknown
  |]

type TypDefK = TypDef' Symbol
type FldDefK = FldDef' Symbol
type TabDefK = TabDef' Symbol
type RelDefK = RelDef' Symbol

type TypDef = TypDef' Text
type FldDef = FldDef' Text
type TabDef = TabDef' Text
type RelDef = RelDef' Text

-- CTypDef
-- | instances will be generated by TH
class
  (ToStar name, ToStar (TTypDef sch name)) => CTypDef sch (name :: Symbol) where

  type TTypDef sch name :: TypDefK

typDef :: forall sch name. CTypDef sch name => TypDef
typDef = toStar @_ @(TTypDef sch name)
genDefunSymbols [''TTypDef]

-- CFldDef
-- | instances will be generated by TH
class
  ( ToStar fname, ToStar tname
  , ToStar (TFldDef sch tname fname)
  , CTypDef sch (FdType (TFldDef sch tname fname)) )
  => CFldDef sch (tname::Symbol) (fname::Symbol) where
  type TFldDef sch tname fname :: FldDefK

fldDef :: forall sch tname fname. CFldDef sch tname fname => FldDef
fldDef = toStar @_ @(TFldDef sch tname fname)

genDefunSymbols [''TFldDef]

type family
  CFldDefs (sch::Type) (tab::Symbol) (flds::[Symbol]) :: Constraint where
  CFldDefs sch tab '[] = ()
  CFldDefs sch tab (x ': xs) = (CFldDef sch tab x, CFldDefs sch tab xs)


-- CTabDef
-- | instances will be generated by TH
class
  ( ToStar name, ToStar (TTabDef sch name)
  , ToStar (SP.Map (TFldDefSym2 sch name) (TdFlds (TTabDef sch name)))
  , ToStar (SP.Map (TFldDefSym2 sch name) (TdKey (TTabDef sch name)))
  , ToStar
    ( ConcatMap (MapSym1 (TFldDefSym2 sch name)) (TdUniq (TTabDef sch name)) )
  ) => CTabDef sch (name::Symbol) where

  type TTabDef sch name :: TabDefK

-- CRelDef
-- | instances will be generated by TH
class
  ( ToStar name, ToStar (TRelDef sch name)
  , CTabDef sch (TFromTab sch name)
  , CTabDef sch (TToTab sch name)
  , ToStar (SP.Map (TFldDefSym2 sch (TFromTab sch name)) (TFromFlds sch name))
  , ToStar (SP.Map (TFldDefSym2 sch (TToTab sch name)) (TToFlds sch name))
  )
  => CRelDef sch (name::Symbol) where

  type TRelDef sch name :: RelDefK

type TFromTab sch name = RdFrom (TRelDef sch name)
type TFromFlds sch name = SP.Map FstSym0 (RdCols (TRelDef sch name))
type TToTab sch name = RdTo (TRelDef sch name)
type TToFlds sch name = SP.Map SndSym0 (RdCols (TRelDef sch name))

genDefunSymbols [''TTabDef, ''TRelDef]
-- we can also defun CTabDef and CRelDef but this is not needed

type TTabDefs sch = SP.Map (TTabDefSym1 sch) (TTabs sch)
type TTabFlds sch = SP.Map TdFldsSym0 (TTabDefs sch)
type TTabFldDefs sch =
  Zip2With (TFldDefSym1 sch) (TTabs sch) (TTabFlds sch)

type TFieldKind sch tab name =
  GetFldKind (TTabDef sch tab) (TRelDefs sch) tab name

-- | instances will be generated by TH
class
  ( ToStar (TSchema sch), ToStar (TTabs sch), ToStar (TRels sch)
  , ToStar (SP.Map (TTabDefSym1 sch) (TTabs sch))
  , ToStar (SP.Map (TRelDefSym1 sch) (TRels sch))
  , ToStar (TTabFldDefs sch)
  , ToStar (TTabFlds sch)
  , ToStar (TTabDefs sch)
  , ToStar (TTypes sch)
  , ToStar (SP.Map (TTypDefSym1 sch) (TTypes sch))
  )
  => CSchema sch where

  type TSchema sch  :: Symbol
  type TTabs sch    :: [Symbol]
  type TRels sch    :: [Symbol]
  type TTypes sch   :: [Symbol]

type TRelDefs sch = Map2 (TRelDefSym1 sch) (TRels sch)

schemaName :: forall sch. CSchema sch => Text
schemaName = toStar @_ @(TSchema sch)
--
tabDefMap :: forall sch. CSchema sch => M.Map Text (TabDef, M.Map Text FldDef)
tabDefMap = M.fromList
  $ L.zip tabs
    $ L.zip tabDefs
      $ L.zipWith (\fs -> M.fromList . L.zip fs) tabFlds tabFldDefs
  where
    tabs = toStar @_ @(TTabs sch)
    tabDefs = toStar @_ @(TTabDefs sch)
    tabFlds = toStar @_ @(TTabFlds sch)
    tabFldDefs = toStar @_ @(TTabFldDefs sch)

--
relDefMap :: forall sch. CSchema sch => M.Map Text RelDef
relDefMap = M.fromList $ L.zip
  (toStar @_ @(TRels sch)) $ toStar @_ @(SP.Map (TRelDefSym1 sch) (TRels sch))

typDefMap :: forall sch. CSchema sch => M.Map Text TypDef
typDefMap = M.fromList $ L.zip
  (toStar @_ @(TTypes sch))
  (toStar @_ @(SP.Map (TTypDefSym1 sch) (TTypes sch)))

#if !MIN_VERSION_base(4,11,0)
type (:====) a b = (:==) a b
#else
type (:====) a b = (==) a b
#endif

type family TabOnPath sch (t :: Symbol) (path :: [Symbol]) :: Symbol where
  TabOnPath sch t '[] = t
  TabOnPath sch t (x ': xs) = TabOnPath sch
    (If (TFromTab sch x :==== t) (TToTab sch x) (TFromTab sch x)) xs
--
type family TabPath sch (t :: Symbol) (path :: [Symbol]) :: Constraint where
  TabPath sch t '[] = ()
  TabPath sch t (x ': xs) = TabPath sch
    (If (TFromTab sch x :==== t) (TToTab sch x) (TFromTab sch x)) xs
