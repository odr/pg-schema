{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Condition where

import Data.Aeson (FromJSON(..), ToJSON(..))
-- import Data.Kind (Type)
import Control.Monad.Reader
import Control.Monad.State
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Database.PostgreSQL.Convert
import Database.PostgreSQL.Simple.ToField
import Database.Schema.Def
import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)


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
data Cond sch (tab::Symbol)
  = Empty
  | forall fld v .
    ( CFldDef sch tab fld
    , Show v
    , CanConvertPG sch (FdType (TFldDef sch tab fld))
      (FdNullable (TFldDef sch tab fld)) v )
    => Cmp (Proxy fld) Cmp v
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
    ( CFldDef sch tab name, Show v
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
  ( CFldDef sch tab fld, Show v
  , CanConvertPG sch (FdType (TFldDef sch tab fld))
    (FdNullable (TFldDef sch tab fld)) v )
  => IsLabel fld (Cmp -> v -> Cond sch tab) where
  fromLabel = Cmp @sch @tab @fld Proxy
--
pnot :: Cond sch tab -> Cond sch tab
pnot = Not

(&&&), (|||) :: Cond sch tab -> Cond sch tab -> Cond sch tab
(&&&) = BoolOp And
(|||) = BoolOp Or
infixl 2 |||
infixl 3 &&&
--
(<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?)
  :: (Cmp -> v -> Cond sch tab) -> v -> Cond sch tab
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
--
-- <номер таблицы родителя> <номер дочерней таблицы>
type CondMonad = ReaderT Int (State Int)
runCond :: CondMonad a -> a
runCond x = evalState (runReaderT x 0) 0

data SomeToField where
  SomeToField :: ToField a => a -> SomeToField

instance ToField SomeToField where
  toField (SomeToField v) = toField v
--
-- withPar :: (Text -> a) -> CondMonad a
-- withPar f = lift $ do
--   n <- snd <$> get
--   modify $ second (+1)
--   return $ f (paramName @b n)
--
convCond :: Cond sch t -> CondMonad (Text, [SomeToField])
convCond (condition :: Cond sch t) = case condition of
  Empty -> return ("1=1",[])
  -- (Cmp (_::Proxy n) cmp v) -> do
  --   ntab <- get
  --   first (T.intercalate " AND ") . unzip <$> mapM (\(n,vdb) ->
  --     let nm = format "tw{}.{}" (ntab, n) in
  --       withPar @db $ \par ->
  --         (case cmp of
  --           Like    -> condLike @db nm par
  --           CmpS op -> format "{}{}{}" (nm,showCmp op,par)
  --         , vdb
  --         )
  --     ) nvs
  --   where
  --     nvs =
        -- zip (map fst $ fldDbDef @db @n @(TabFldType sch t n)) $ fldToDb @db @n v
--   Null (_::Proxy n) -> return $ (,[])
--     $ T.intercalate " AND "
--     $ map ((`mappend` " IS NULL") . fst)
--     $ fldDbDef @db @n @(TabFldType sch t n)
--   Not c -> first (format "NOT ({})" . Only) <$> convCond c
--   BoolOp bo c1 c2 ->
--     (\(cc1,d1) (cc2,d2) -> (format "({}) {} ({})" (cc1,show bo,cc2),d1++d2))
--       <$> convCond c1 <*> convCond c2
--   Child (_ :: Proxy ref) (cond::Cond db sch (RdFrom (TRelDef sch ref))) ->
--     getRef True (tabName @sch @(RdFrom (TRelDef sch ref)))
--       (toStar @_ @(TRelDef sch ref)) (convCond cond)
--   Parent (_ :: Proxy ref) (cond::Cond db sch (RdTo (TRelDef sch ref))) ->
--     getRef False (tabName @sch @(RdTo (TRelDef sch ref)))
--       (toStar @_ @(TRelDef sch ref)) (convCond cond)
--   where
--     getRef
--       :: Bool -> Text -> RelDef Text -> CondMonad (Text, [FieldToDB db])
--       -> CondMonad (Text, [FieldToDB db])
--     getRef isChild tn rd cc = do
--       pnum <- ask
--       cnum <- lift $ do
--         modify (first (+1))
--         fst <$> get
--
--       first (\c ->
--         format "EXISTS (SELECT 1 FROM {} t{} WHERE {}{}({}))"
--           ( tn
--           , cnum
--           , T.intercalate " AND "
--             $ map
--               ( (\(ch,pr) -> format "t{}.{} = t{}.{}" (cnum,ch,pnum,pr))
--                 . (if isChild then id else swap) )
--             $ rdCols rd
--           , if T.null c then (""::T.Text) else " AND "
--           , c)
--         ) <$> local (const cnum) cc
  _     -> undefined

-- --
-- -- > dbCond $ pcmp @"id" @Dbs @Sch @"Customer" ==? 1 ||| pchild @"ordCust" (#id ==? 2 ||| pnull @"num")
-- -- ("(t0.id=?1) Or (EXISTS (SELECT 1 FROM Orders t1 WHERE t1.customerId = t0.id AND ((t1.id=?2) Or (num IS NULL))))",[SQLInteger 1,SQLInteger 2])
-- -- > dbCond $ pcmp @"id" @Dbs @Sch @"Orders" ==? 1 &&& pparent @"ordCust" (#id ==? 2) &&& pchild @"opOrd" (#num >? 3)
-- -- ("((t0.id=?1) And (EXISTS (SELECT 1 FROM Customer t1 WHERE t1.id = t0.customerId AND (t1.id=?2)))) And (EXISTS (SELECT 1 FROM OrderPosition t2 WHERE t2.orderId = t0.id AND (t2.num>?3)))",[SQLInteger 1,SQLInteger 2,SQLInteger 3])
-- -- > dbCond $ #id ==? 1 &&& pparent @"ordCust" @Dbs @Sch @"Orders" (#id ==? 2) &&& pchild @"opOrd" (#num >? 3)
-- -- ("((t0.id=?1)
--   -- And (EXISTS (SELECT 1 FROM Customer t1 WHERE t1.id = t0.customerId AND (t1.id=?2))))
--   -- And (EXISTS (SELECT 1 FROM OrderPosition t2 WHERE t2.orderId = t0.id AND (t2.num>?3)))"
--   -- ,[SQLInteger 1,SQLInteger 2,SQLInteger 3])
--
--
-- dbCond :: Cond db sch t -> (Text, [FieldToDB db])
-- dbCond = runCond . convCond
