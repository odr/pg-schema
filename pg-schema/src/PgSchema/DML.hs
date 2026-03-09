-- | A module to generate and safely execute SQL statements.
--
-- = TLDR; Examples:
--
-- @
-- data Order = Order { num :: Text, created_at :: Day, items :: [OrdPos] } deriving Generic
-- data OrdPos = OrdPos { id :: Int32, num :: Int32, article :: Article, price :: Double } deriving Generic
-- data Article = Article { id :: Int32, name :: Text } deriving Generic
--
-- ...
-- do
--   void $ insertJSON_ RenamerId Sch "orders" conn
--     [ "num" =: "num1" :. "ord__ord_pos" =:
--       [ "num" =: 1 :. "article_id" =: 42 :. "price" =: 10
--       , "num" =: 2 :. "article_id" =: 41 :. "price" =: 15 ]
--     ]
--   (xs :: [Order]) <- selectSch RenamerId Sch "orders" conn $ qRoot do
--     qWhere $ "created_at" >? someDay
--        &&& pchild "ord__ord_pos" defTabParam
--          (pparent "ord_pos__article" $ "name" ~~? "%pencil%")
--     qPath "ord__ord_pos" do
--       qWhere $ pparent "ord_pos__article" $ "name" ~~? "%pencil%"
-- @
--
-- We get here all orders created after `someDay` having positions with articles like "%pencil%"
-- and return also only those orders items that relates to "pencil" with info about article.
--
-- We can use both 'GHC.Generics.Generic'-based and 'Data.Tagged.Tagged'-base ('(PgSchema.Types.=:)') interface and mix them in any way
--
-- Note that all "strings" here are 'GHC.TypeLits.Symbol' due to @RequiredTypeArguments@ extension.
-- And operations are safe if database schema is correct.
module PgSchema.DML
  (
  -- * Select
  -- ** Execute SQL
    selectSch, selectText
  -- ** Monad to set Query Params
  -- *** Base definitions
  , QueryParam(..), CondWithPath(..), OrdWithPath(..), LimOffWithPath(..), DistWithPath(..)
  -- *** Monad to set Query Params
  , MonadQP, qpEmpty, qRoot, qPath, qWhere, qOrderBy, qDistinct, qDistinctOn, qLimit, qOffset
  -- ** Conditions
  -- *** Base definitions
  , Cond(..), Cmp(..), BoolOp(..), CDBField, CDBValue, CDBFieldNullable
  , CDBParent, CDBChild
  -- *** Conditions EDSL
  , (<?),(>?),(<=?),(>=?),(=?),(~=?),(~~?), (|||), (&&&), pnot, pnull, pin, pinArr
  , pUnsafeCond, pparent, pchild
  -- *** TabParam for child
  , TabParam(..), defTabParam
  -- *** Internals for 'UnsafeCond'
  , CondMonad, SomeToField, showCmp
  -- ** Order By and others
  , OrdDirection(..), OrdFld(..), Dist(..), LO(..), ordf, ascf, descf, defLO, lo1
  -- * Plain Insert
  -- ** Execute SQL
  , insertSch, insertSch_, insertText, insertText_
  -- ** Constraints
  , InsertReturning', InsertNonReturning'
  -- * Update
  -- ** Execute SQL
  , updateByCond, updateByCond_, updateText, updateText_
  -- ** Constraints
  , UpdateReturning, HListInfo, HRep
  -- * Tree-base Insert/Upsert
  -- ** Execute SQL
  , insertJSON, insertJSON_, upsertJSON, upsertJSON_, insertJSONText, insertJSONText_
  -- ** Constraints
  , InsertReturning, InsertNonReturning, UpsertReturning, UpsertNonReturning
  , SrcJSON, TgtJSON
  -- * Delete
  , deleteByCond, deleteText
  ) where

import PgSchema.DML.Select as S
import PgSchema.DML.Select.Types as S
import PgSchema.DML.Insert as I
import PgSchema.DML.Insert.Types as I
import PgSchema.DML.InsertJSON as I
import PgSchema.DML.Update as U
import PgSchema.DML.Delete as D
