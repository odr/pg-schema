-- DuplicateRecordFields conflicts with singletons!
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances    #-}
module Database.Schema.Def where

import Data.List as L
import Data.Map as M
-- import Data.Singletons
import Data.Singletons.TH
import Data.Text as T
import Database.Schema.Util.ToStar
import GHC.TypeLits
import Type.Reflection (Typeable, typeRep)


data DelCons = DcRestrict | DcCascade | DcSetNull
  deriving (Show, Eq, Ord, Read, Typeable)

promote [d|
  data TypDef s = TypDefC
    { typCategory :: s
    , typElem     :: Maybe s
    , typEnum     :: [s] }
    deriving Show

  data FldDef s = FldDefC
    { fdType        :: s
    , fdNullable    :: Bool
    , fdHasDefault  :: Bool}
    deriving Show

  data TabDef s = TabDefC
    { tdFlds       :: [s]
    , tdKey        :: [s]
    , tdUniq       :: [[s]]
    , tdInsertable :: Bool }
    deriving Show

  data RelDef s = RelDefC
    { rdFrom    :: s
    , rdTo      :: s
    , rdCols    :: [(s,s)]
    , rdDelCons :: DelCons }
    deriving Show
  |]

type instance TStar DelCons = DelCons

instance Typeable s => ToStar (s::DelCons) where
  toStar = read $ L.drop 1 $ show $ typeRep @s

type instance TStar (TypDef Symbol) = TypDef Text
type instance TStar (FldDef Symbol) = FldDef Text
type instance TStar (TabDef Symbol) = TabDef Text
type instance TStar (RelDef Symbol) = RelDef Text

instance (ToStar a, ToStar b, ToStar c)
  => ToStar ('TypDefC a b c :: TypDef Symbol) where
    toStar = TypDefC (toStar @_ @a) (toStar @_ @b) (toStar @_ @c)

instance (ToStar t, ToStar n, ToStar d)
  => ToStar ('FldDefC t n d :: FldDef Symbol) where
  toStar = FldDefC (toStar @_ @t) (toStar @_ @n) (toStar @_ @d)

instance (ToStar fs, ToStar pk, ToStar uk, ToStar ins)
  => ToStar ('TabDefC fs pk uk ins :: TabDef Symbol) where
  toStar =
    TabDefC (toStar @_ @fs) (toStar @_ @pk) (toStar @_ @uk) (toStar @_ @ins)

instance (ToStar f, ToStar t, ToStar cs, ToStar cons)
  => ToStar ('RelDefC f t cs cons :: RelDef Symbol) where
  toStar =
    RelDefC (toStar @_ @f) (toStar @_ @t) (toStar @_ @cs) (toStar @_ @cons)

-- CFldDef
class
  ( ToStar fname, CTabDef sch tname, ToStar fname
  , ToStar (TFldDef sch tname fname) )
  => CFldDef sch tname (fname::Symbol) where
    type TFldDef sch tname fname :: FldDef Symbol

    fldName :: Text
    fldName = toStar @_ @fname

    fldDef :: FldDef Text
    fldDef = toStar @_ @(TFldDef sch tname fname)

-- CTabDef
class
  ( ToStar name, ToStar (TTabDef sch name)
  , ToStar (TRelListFrom sch name), ToStar (TRelListTo sch name)
  ) => CTabDef sch (name::Symbol) where

  type TTabDef sch name      :: TabDef Symbol
  type TRelListFrom sch name :: [Symbol]
  type TRelListTo sch name   :: [Symbol]

  tabName :: Text
  tabName = toStar @_ @name

  tabDef :: TabDef Text
  tabDef = toStar @_ @(TTabDef sch name)

  relListFrom :: [Text]
  relListFrom = toStar @_ @(TRelListFrom sch name)

  relListTo :: [Text]
  relListTo = toStar @_ @(TRelListTo sch name)

-- CRelDef
class (ToStar name, ToStar (TRelDef sch name))
  => CRelDef sch (name::Symbol) where

  type TRelDef sch name :: RelDef Symbol

  relName :: T.Text
  relName = toStar @_ @name

  relDef :: RelDef T.Text
  relDef = toStar @_ @(TRelDef sch name)

type family TabDefs sch (xs::[Symbol]) :: [(Symbol,TabDef Symbol)] where
  TabDefs sch '[] = '[]
  TabDefs sch (x ': xs) = '(x, TTabDef sch x) ': TabDefs sch xs

type family RelDefs sch (xs::[Symbol]) :: [(Symbol,RelDef Symbol)] where
  RelDefs sch '[] = '[]
  RelDefs sch (x ': xs) = '(x, TRelDef sch x) ': RelDefs sch xs

-- CSchema
class
  ( ToStar (TSchema sch), ToStar (TTables sch), ToStar (TRels sch)
  , ToStar (TabDefs sch (TTables sch)), ToStar (RelDefs sch (TRels sch)))
  => CSchema sch where

  type TSchema sch  :: Symbol
  type TTables sch  :: [Symbol]
  type TRels sch    :: [Symbol]

  tables :: Map T.Text (TabDef T.Text)
  tables = M.fromList (toStar @_ @(TabDefs sch (TTables sch)))

  rels :: Map T.Text (RelDef T.Text)
  rels = M.fromList (toStar @_ @(RelDefs sch (TRels sch)))

-- CTypDef
class ToStar (TTypDef sch name) => CTypDef sch (name :: Symbol) where
  type TTypDef sch name :: TypDef Symbol
