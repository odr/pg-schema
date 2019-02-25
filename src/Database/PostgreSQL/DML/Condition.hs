{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Condition where

import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Bifunctor
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Singletons.Prelude
import Data.Text.Lazy as T
import Data.Tuple
import Database.PostgreSQL.Convert
import Database.PostgreSQL.Simple.ToField
import Database.Schema.Def
import Formatting
import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)
import Util.ToStar
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif


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
data Cond (sch::Type) (tab::Symbol)
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
    => In (Proxy fld) [v]
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
  :: forall name sch tab rel .
    ( rel ~ TRelDef sch name, tab ~ RdFrom rel
    , CTabDef sch (RdTo rel), CRelDef sch name )
  => Cond sch (RdTo rel) -> Cond sch tab
pparent = Parent @sch @tab @name Proxy

-- --
instance
  ( CFldDef sch tab fld, Show v, ToField v
  , CanConvertPG sch (FdType (TFldDef sch tab fld))
    (FdNullable (TFldDef sch tab fld)) v )
  => IsLabel fld (Cmp -> v -> Cond sch tab) where
  fromLabel = Cmp @_ @_ @fld Proxy
--

pnot :: Cond sch tab -> Cond sch tab
pnot = Not

pin
  :: forall name sch tab v .
    ( CFldDef sch tab name, Show v, ToField v
    , CanConvertPG sch (FdType (TFldDef sch tab name))
      (FdNullable (TFldDef sch tab name)) v )
  => [v] -> Cond sch tab
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
type CondMonad = ReaderT Int (State Int)
runCond :: CondMonad a -> a
runCond x = evalState (runReaderT x 0) 0

data SomeToField where
  SomeToField :: (ToField a, Show a) => a -> SomeToField

deriving instance Show SomeToField

instance ToField SomeToField where
  toField (SomeToField v) = toField v
--
convCond :: forall sch t. Cond sch t -> CondMonad (Text, [SomeToField])
convCond = \case
  EmptyCond -> pure mempty
  Cmp (_::Proxy n) cmp v -> go <$> ask
    where
      go ntab = case cmp of
        Like True  -> (fld' <> " like ?", [v'])
        Like False -> ("upper(" <> fld' <> ") like upper(?)", [v'])
        op         -> (fld' <> " " <> showCmp op <> " ?", [v'])
        where
          v' = SomeToField v
          fld' = fld ntab (toStar @_ @n)
  In (_::Proxy n) vs -> go <$> ask
    where
      go ntab =
        ( fld ntab (toStar @_ @n)
          <> " in (" <> (T.intercalate "," $ const "?" <$> vs) <> ")"
        , SomeToField <$> vs )
  Null (_::Proxy n) ->
    (\ntab ->
      (format (text % int % "." % stext % " is null")
        (tabPref ntab) ntab (toStar @_ @n), []))
    <$> ask
  Not c -> getNot <$> convCond c
  BoolOp bo c1 c2 -> getBoolOp bo <$> convCond c1 <*> convCond c2
  Child (_ :: Proxy ref) (cond::Cond sch (RdFrom (TRelDef sch ref))) ->
    getRef True (toStar @_ @(RdFrom (TRelDef sch ref)))
      (toStar @_ @(TRelDef sch ref)) (convCond cond)
  Parent (_ :: Proxy ref) (cond::Cond sch (RdTo (TRelDef sch ref))) ->
    getRef False (toStar @_ @(RdTo (TRelDef sch ref)))
      (toStar @_ @(TRelDef sch ref)) (convCond cond)
  where
    tabPref ntab
      | ntab == 0 = "t"
      | otherwise = "qt"
    fld ntab name = format (text % int % "." % stext) (tabPref ntab) ntab name
    getNot (c,vs)
      | c == mempty = mempty
      | otherwise   = (format ("not (" % text % ")") c, vs)
    getBoolOp bo (cc1,d1) (cc2,d2)
      | cc1 == mempty = (cc2,d2)
      | cc2 == mempty = (cc1,d1)
      | otherwise =
        (format ("(" % text % ") " % string % " (" % text % ")")
          cc1 (show bo) cc2
        , d1++d2)

    getRef isChild tn rd cc = do
      pnum <- ask
      cnum <- lift $ do
        modify (+1)
        get

      first (\c ->
        format ("exists (select 1 from " % stext % " qt" % int %
          " where " % text % ")")
          tn cnum
          (T.intercalate " and "
            (
              ( (\(ch,pr) -> format
                ( "qt" % int % "." % stext % " = " % text % int % "." % stext)
                  cnum ch (tabPref pnum) pnum pr)
                . (if isChild then id else swap) )
            <$> rdCols rd)
            <> (if T.null c then (""::T.Text) else (" and (" <> c <> ")")))
        ) <$> local (const cnum) cc

pgCond :: Cond sch t -> (Text, [SomeToField])
pgCond = runCond . convCond

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
  = forall (path :: [Symbol]) tab. PathToTab sch t path tab
  => CondWithPath (Proxy path) (Cond sch tab)

class PathToTab sch t path tab

instance PathToTab sch t '[] t

#if !MIN_VERSION_base(4,11,0)
instance
  ( t' ~ If (TFromTab sch x :== t) (TToTab sch x) (TFromTab sch x)
  , PathToTab sch t' xs tab
  )
  => PathToTab sch t (x ': xs) tab
#else
instance
  ( t' ~ If (TFromTab sch x == t) (TToTab sch x) (TFromTab sch x)
  , PathToTab sch t' xs tab
  )
  => PathToTab sch t (x ': xs) tab
#endif
