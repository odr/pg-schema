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
import qualified Data.Text as T.S
import Data.Text.Lazy as T
import Data.Tuple
import Database.PostgreSQL.Simple.ToField
import Formatting
import GHC.Generics (Generic)
-- import           GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits

import Database.PostgreSQL.Convert
import Database.Schema.Def
import Database.Schema.ShowType
import PgSchema.Util


data Cmp = (:=) | (:<=) | (:>=) | (:>) | (:<) | Like {isCaseSesnsitive :: Bool}
  deriving (Show, Eq, Generic)

instance FromJSON Cmp
instance ToJSON Cmp

data BoolOp = And | Or
  deriving (Show, Eq, Generic)

instance FromJSON BoolOp
instance ToJSON BoolOp

showCmp :: Cmp -> Text
showCmp = \case
  (:=)    -> "="
  (:<=)   -> "<="
  (:>=)   -> ">="
  (:<)    -> "<"
  (:>)    -> ">"
  Like _  -> " like "

-- https://github.com/emmanueljs1/ghc-proposals/blob
-- /5a685faf899a2b00361b221d7e945a4922bf7863
-- /existental-type-variables.rst#implementation-plan
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
    => Child (Proxy ref) (Cond sch (RdFrom rel))
  --
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
  => Cond sch (RdFrom rel) -> Cond sch tab
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

fld
  :: forall fld sch tab v
    . ( CFldDef sch tab fld, Show v, ToField v
      , CanConvertPG sch (FdType (TFldDef sch tab fld))
        (FdNullable (TFldDef sch tab fld)) v )
  => Cmp -> v -> Cond sch tab
fld = Cmp @_ @_ @fld Proxy

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
x ~=? b  = x (Like True) b
x ~~? b  = x (Like False) b
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

-- <номер таблицы родителя> <номер дочерней таблицы>
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
  :: forall sch t. CSchema sch => Maybe Int -> Cond sch t -> CondMonad Text
convCond rootTabNum = \case
  EmptyCond -> pure mempty
  Cmp (_::Proxy n) cmp v -> ask >>= go
    where
      go ntab = do
        tell [SomeToField v]
        pure $ case cmp of
          Like True  -> fldt' <> " like ?"
          Like False -> "upper(" <> fldt' <> ") like upper(?)"
          op         -> fldt' <> " " <> showCmp op <> " ?"
        where
          fldt' = fldt ntab (demote @n)
  In (_::Proxy n) (toList -> vs) -> tell (SomeToField <$> vs) >> go <$> ask
    where
      go ntab = fldt ntab (demote @n)
          <> " in (" <> T.intercalate "," ("?" <$ vs) <> ")"
  Null (_::Proxy n) ->
    (\ntab -> format (text % int % "." % stext % " is null")
      (tabPref ntab) ntab (demote @n))
    <$> ask
  Not c -> getNot <$> convCond rootTabNum c
  BoolOp bo c1 c2 ->
    getBoolOp bo <$> convCond rootTabNum c1 <*> convCond rootTabNum c2
  Child (_ :: Proxy ref) (cond::Cond sch (RdFrom (TRelDef sch ref))) ->
    getRef True (demote @(RdFrom (TRelDef sch ref)))
      (demote @(TRelDef sch ref)) (convCond rootTabNum cond)
  Parent (_ :: Proxy ref) (cond::Cond sch (RdTo (TRelDef sch ref))) ->
    getRef False (demote @(RdTo (TRelDef sch ref)))
      (demote @(TRelDef sch ref)) (convCond rootTabNum cond)
  where
    tabPref ntab
      | ntab == 0 = maybe "t" (format $ "t" % int) rootTabNum
      | otherwise = maybe
        (format $ "tq" % int) (format $ "t" % int % "q" % int) rootTabNum ntab
    fldt ntab = format (text % "." % stext) (tabPref ntab)
    getNot c
      | c == mempty = mempty
      | otherwise   = format ("not (" % text % ")") c
    getBoolOp bo cc1 cc2
      | cc1 == mempty = cc2
      | cc2 == mempty = cc1
      | otherwise = format ("(" % text % ") " % string % " (" % text % ")")
        cc1 (show bo) cc2
    getRef :: Bool -> NameNS -> RelDef -> CondMonad Text -> CondMonad Text
    getRef isChild tn rd cc = do
      pnum <- ask
      modify (+1)
      cnum <- get
      mkExists pnum cnum <$> local (const cnum) cc
      where
        mkExists pnum cnum c =
          format ("exists (select 1 from " % stext % " " % text %
            " where " % text % ")")
            (qualName tn) (tabPref cnum)
            (T.intercalate " and "
              (
               (\(ch,pr) -> format
                (text % "." % stext % " = " % text % "." % stext)
                  (tabPref cnum) ch (tabPref pnum) pr)
                . (if isChild then id else swap)
              <$> rdCols rd)
              <> if T.null c then (""::T.Text) else " and (" <> c <> ")")

pgCond
  :: forall sch t. CSchema sch
  => Maybe Int -> Cond sch t -> (Text, [SomeToField])
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
  -> [T.S.Text] -> CondWithPath sch t -> Maybe r
withCondWithPath f path (CondWithPath (Proxy :: Proxy path') cond) =
  guard (path == demote @path') >> pure (f cond)

withCondsWithPath
  :: forall sch t r
  . (forall t'. Cond sch t' -> r)
  -> [T.S.Text] -> [CondWithPath sch t] -> Maybe r
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
  => Int -> [T.S.Text] ->[CondWithPath sch t] -> (Text, [SomeToField])
condByPath num path =
  fromMaybe mempty . withCondsWithPath (pgCond $ Just num) path
