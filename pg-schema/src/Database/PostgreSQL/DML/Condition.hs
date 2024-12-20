{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Condition where

import Control.Monad.RWS
import Control.Monad
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Kind (Type)
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Singletons
import qualified Data.Text as T
-- import Data.Text.Lazy as T
import Data.Tuple
import Database.PostgreSQL.DML.Limit
import Database.PostgreSQL.DML.Order
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)
-- import           GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits

import Database.PostgreSQL.Convert
import Database.Schema.Def
import Database.Schema.ShowType
import PgSchema.Util


data Cmp = (:=) | (:<=) | (:>=) | (:>) | (:<) | Like | ILike
  deriving (Show, Eq, Generic)

instance FromJSON Cmp
instance ToJSON Cmp

data BoolOp = And | Or
  deriving (Show, Eq, Generic)

instance FromJSON BoolOp
instance ToJSON BoolOp

showCmp :: Cmp -> T.Text
showCmp = \case
  (:=)  -> "="
  (:<=) -> "<="
  (:>=) -> ">="
  (:<)  -> "<"
  (:>)  -> ">"
  Like  -> "like"
  ILike -> "ilike"

-- https://github.com/emmanueljs1/ghc-proposals/blob/5a685faf899a2b00361b221d7e945a4922bf7863/existental-type-variables.rst#implementation-plan
-- we have to add Proxy to existensials while ^ this proposal isn't implemented
data Cond (sch::Type) (tab::NameNSK)
  = EmptyCond
  | forall fld v .
    ( CFldDef sch tab fld
    , ToField v, Show v
    , CanConvertPG sch (FdType (TFldDef sch tab fld))
      (FdNullable (TFldDef sch tab fld)) v )
    => Cmp (Proxy fld) Cmp v
  | forall fld v .
    ( CFldDef sch tab fld
    , ToField v, Show v
    , CanConvertPG sch (FdType (TFldDef sch tab fld))
      (FdNullable (TFldDef sch tab fld)) v )
    => In (Proxy fld) (NonEmpty v)
  | forall fld .
    (CFldDef sch tab fld, FdNullable (TFldDef sch tab fld) ~ 'True)
    => Null (Proxy fld)
  | Not (Cond sch tab)
  | BoolOp BoolOp (Cond sch tab) (Cond sch tab)
  -- condition "EXIST"
  | forall ref rel .
    ( rel ~ TRelDef sch ref
    , tab ~ RdTo rel
    , CTabDef sch (RdFrom rel)
    , CRelDef sch ref )
    => Child (Proxy ref) (TabParam sch (RdFrom rel)) (Cond sch (RdFrom rel))
  -- condition "JOIN"
  | forall ref rel .
    ( rel ~ TRelDef sch ref
    , tab ~ RdFrom rel
    , CTabDef sch (RdTo rel)
    , CRelDef sch ref )
    => Parent (Proxy ref) (Cond sch (RdTo rel))
--
deriving instance Show (Cond sch tab)

instance Semigroup (Cond sch tab) where
  c1 <> c2 = c1 &&& c2

instance Monoid (Cond sch tab) where
  mempty = EmptyCond

data TabParam sch tab = TabParam
  { cond :: Cond sch tab
  , order :: [OrdFld sch tab]
  , lo :: LO }
  deriving Show

defTabParam :: TabParam sch tab
defTabParam = TabParam mempty mempty defLO

--
pcmp
  :: forall name sch tab v .
    ( CFldDef sch tab name, Show v, ToField v
    , CanConvertPG sch (FdType (TFldDef sch tab name))
      (FdNullable (TFldDef sch tab name)) v )
  => Cmp -> v -> Cond sch tab
pcmp = Cmp @sch @tab @name Proxy

pnull
  :: forall name sch tab .
    (CFldDef sch tab name, FdNullable (TFldDef sch tab name) ~ 'True)
  => Cond sch tab
pnull = Null @sch @tab @name Proxy

pchild
  :: forall name sch tab rel .
    ( rel ~ TRelDef sch name, tab ~ RdTo rel
    , CTabDef sch (RdFrom rel), CRelDef sch name )
  => TabParam sch (RdFrom rel) -> Cond sch (RdFrom rel) -> Cond sch tab
pchild = Child @sch @tab @name Proxy

pparent
  :: forall name sch rel.
    ( rel ~ TRelDef sch name, CTabDef sch (RdTo rel), CRelDef sch name )
  => Cond sch (RdTo rel) -> Cond sch (RdFrom rel)
pparent = Parent @sch @(RdFrom rel) @name Proxy

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

pin
  :: forall name sch tab v .
    ( CFldDef sch tab name, Show v, ToField v
    , CanConvertPG sch (FdType (TFldDef sch tab name))
      (FdNullable (TFldDef sch tab name)) v )
  => NonEmpty v -> Cond sch tab
pin = In @sch @tab @name Proxy

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
--
-- ghci> :t pcmp @"id" @Sch @"customers" =? (1::Int) ||| pchild @"ord_cust"
-- (#id =? (1::Int) ||| pnot (pchild @"opos_order" (#price >? 5)))
-- ...
--   :: Cond Sch "customers"

-- or:
-- ghci> :t #id =? (1::Int) ||| pchild @"ord_cust"
-- (#id =? (1::Int) ||| pnot (pchild @"opos_order" (#price >? 5))) :: Cond Sch "customers"

-- or:
-- ghci> x = 1 :: Int
-- ghci> y = 5 :: Integer
-- ghci> :t #id =? x ||| pchild @"ord_cust" (#id =? x||| pnot (pchild @"opos_order" (#price >? y)))
-- :: Cond Sch "customers"

-- <номер таблицы родителя> <list of params> <номер дочерней таблицы>
type CondMonad = RWS Int [SomeToField] Int
runCond :: CondMonad a -> (a,[SomeToField])
runCond x = evalRWS x 0 0

data SomeToField where
  SomeToField :: (ToField a, Show a) => a -> SomeToField

deriving instance Show SomeToField

instance ToField SomeToField where
  toField (SomeToField v) = toField v

--
convCond
  :: forall sch t. CSchema sch => Maybe Int -> Cond sch t -> CondMonad T.Text
convCond rootTabNum = \case
  EmptyCond -> pure mempty
  Cmp (_::Proxy n) cmp v -> tell [SomeToField v] >> render <$> ask
    where
      render ntab = fldt ntab (demote @n) <> " " <> showCmp cmp <> " ?"
  In (_::Proxy n) (toList -> vs) -> tell (SomeToField <$> vs) >> go <$> ask
    where
      go ntab = fldt ntab (demote @n)
          <> " in (" <> T.intercalate "," ("?" <$ vs) <> ")"
  Null (_::Proxy n) ->
    (\ntab -> tabPref ntab <> show' ntab <> "." <> demote @n <> " is null")
    <$> ask
  Not c -> getNot <$> convCond rootTabNum c
  BoolOp bo c1 c2 ->
    getBoolOp bo <$> convCond rootTabNum c1 <*> convCond rootTabNum c2
  Child (_ :: Proxy ref) tabParam cond -> -- (cond::Cond sch (RdFrom (TRelDef sch ref))) ->
    getRef @(RdFrom (TRelDef sch ref)) True (demote @(RdCols (TRelDef sch ref)))
      tabParam cond
  Parent (_ :: Proxy ref) cond -> -- (cond::Cond sch (RdTo (TRelDef sch ref))) ->
    getRef @(RdTo (TRelDef sch ref)) False (demote @(RdCols (TRelDef sch ref)))
      defTabParam cond
  -- First ord cond ->
  where
    tabPref ntab
      | ntab == 0 = maybe "t" (("t" <>) . show') rootTabNum
      | otherwise = maybe ("tq" <> show' ntab)
        (\rt -> "t" <> show' rt <> "q" <> show' ntab) rootTabNum
    fldt ntab = ((tabPref ntab <> ".") <>)
    getNot c
      | c == mempty = mempty
      | otherwise   = "not (" <> c <> ")"
    getBoolOp bo cc1 cc2
      | cc1 == mempty = cc2
      | cc2 == mempty = cc1
      | otherwise = "(" <> cc1 <> ") " <> show' bo <> " (" <> cc2 <>  ")"
    getRef
      :: forall tab. CTabDef sch tab
      => Bool -> [(T.Text, T.Text)] -> TabParam sch tab -> Cond sch tab
      -> CondMonad T.Text
    getRef isChild cols tabParam cond = do
      pnum <- ask
      modify (+1)
      cnum <- get
      condInternal <- local (const cnum) $ convCond rootTabNum tabParam.cond
      condExternal <- local (const cnum) $ convCond rootTabNum cond
      pure $ mkExists pnum cnum condInternal condExternal
      where
        mkExists pnum cnum cin cout
          = "exists (select 1 from (select * from " <> tn <> " " <> tpc
          <> " where "
          <> T.intercalate " and " (
              (\(ch,pr) -> tpc <> "." <> ch <> " = "
                <> tpp <> "." <> pr)
              . (if isChild then id else swap)
            <$> cols)
          <> (if T.null cin then "" else " and " <> cin)
          <> case tabParam of
            TabParam EmptyCond [] (LO Nothing Nothing) -> ""
            TabParam _ (convOrd tpc -> ord)
              (convLO -> loTxt) ->
              (if T.null ord then "" else " order by " <> ord) <> loTxt
          <> ") " <> tpc
          <> (if T.null cout then "" else " where " <> cout)
          <> ")"
          where
            tn = qualName $ demote @tab
            tpc = tabPref cnum
            tpp = tabPref pnum

pgCond
  :: forall sch t. CSchema sch
  => Maybe Int -> Cond sch t -> (T.Text, [SomeToField])
pgCond n = runCond . convCond n

{-
ghci> pgCond
  (pparent @"cust_addr"
    (pparent @"address_city" (#name ~=? Just @Text "xx" ))
    &&& (#id =? (5::Int)
      ||| pchild @"ord_cust" (#id =? (1::Int)
      ||| pnot (pchild @"opos_order" EmptyCond))) :: Cond Sch "customers")

("(exists (
  select 1 from addresses qt1
    where qt1.id = t0.address_id and (exists (
      select 1 from cities qt2
        where qt2.id = qt1.city_id and (qt2.name like ?))
  ))
  And
    ( (t0.id = ?)
    Or (exists (
      select 1 from orders qt3
        where qt3.customer_id = t0.id
          and
            ( (qt3.id = ?)
            Or (not (exists (
              select 1 from order_positions qt4
                where qt4.order_id = qt3.id)))
    ))))"
,[SomeToField (Just "xx"),SomeToField 5,SomeToField 1])
-}

data CondWithPath sch t
  = forall (path :: [Symbol]) . ToStar path
  => CondWithPath (Proxy path) (Cond sch (TabOnPath sch t path))

withCondWithPath
  :: forall sch t r
  . (forall t'. Cond sch t' -> r)
  -> [T.Text] -> CondWithPath sch t -> Maybe r
withCondWithPath f path (CondWithPath (Proxy :: Proxy path') cond) =
  guard (path == demote @path') >> pure (f cond)

withCondsWithPath
  :: forall sch t r
  . (forall t'. Cond sch t' -> r)
  -> [T.Text] -> [CondWithPath sch t] -> Maybe r
withCondsWithPath f path =
  join . L.find isJust . L.map (withCondWithPath f path)

cwp
  :: forall path sch t t1. (t1 ~ TabOnPath sch t path, ToStar path)
  => Cond sch t1 -> CondWithPath sch t
cwp = CondWithPath (Proxy @path)

rootCond :: forall sch t. Cond sch t -> CondWithPath sch t
rootCond = cwp @'[]

condByPath
  :: forall sch t. CSchema sch
  => Int -> [T.Text] ->[CondWithPath sch t] -> (T.Text, [SomeToField])
condByPath num path =
  fromMaybe mempty . withCondsWithPath (pgCond $ Just num) path
