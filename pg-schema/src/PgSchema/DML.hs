-- |
-- Module: PgSchema.DML
-- Copyright: (c) Dmitry Olshansky
-- License: BSD-3-Clause
-- Maintainer: olshanskydr@gmail.com, dima@typeable.io
-- Stability: experimental
--
-- === A module to generate and safely execute SQL statements.
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
--   void $ insertJSON_ RenamerId Sch ("dbs" ->> "orders") conn
--     [ "num" =: "num1" :. "ord__ord_pos" =:
--       [ "num" =: 1 :. "article_id" =: 42 :. "price" =: 10
--       , "num" =: 2 :. "article_id" =: 41 :. "price" =: 15 ]
--     ]
--   (xs :: [Order]) <- selectSch RenamerId Sch ("dbs" ->> "orders") conn
--     $ qRoot do
--       qWhere $ "created_at" >? someDay
--         &&& pchild "ord__ord_pos" defTabParam
--           (pparent "ord_pos__article" $ "name" ~~? "%pencil%")
--       qOrderBy [descf "created_at", descf "num"]
--       qPath "ord__ord_pos" do
--         qWhere $ pparent "ord_pos__article" $ "name" ~~? "%pencil%"
--         qOrderBy [ascf "num"]
--       qLimit 20
-- @
--
-- Here we get all orders created after `someDay` that have positions with articles like "%pencil%",
-- and return only those order items that relate to "pencil", with article info.
--
-- We can use both 'GHC.Generics.Generic'-based and 'Data.Tagged.Tagged'-based ('(=:)') interfaces and mix them in any way.
--
-- Note that all "strings" here are 'GHC.TypeLits.Symbol' due to @RequiredTypeArguments@ extension.
-- And operations are safe if database schema is correct.
--
-- Here both @INSERT@ and @SELECT@ are single and fast operation in database (using JSON internally).
--
module PgSchema.DML
  (
  -- * Select
  -- ** Execute SQL
    selectSch, selectText, Selectable
  -- ** Monad to set Query Params
  -- *** Base definitions
  , MonadQP, qpEmpty
  , QueryParam(..), CondWithPath(..), OrdWithPath(..), LimOffWithPath(..), DistWithPath(..)
  -- *** Monad functions
  , qRoot, qPath, qWhere, qOrderBy, qDistinct, qDistinctOn, qLimit, qOffset
  -- ** Conditions
  -- *** Base definitions
  , Cond(..), Cmp(..), BoolOp(..)
  -- *** Conditions EDSL
  -- **** Comparing operations.
  -- | Example: @"name" =? "John"@
  , (<?),(>?),(<=?),(>=?),(=?)
  -- **** Like and ILike.
  -- | Example: @"name" ~~? "%joh%"@
  ,(~=?),(~~?)
  -- **** Boolean operations
  , (|||), (&&&), pnot
  -- **** Parent and child conditions
  , pparent, pchild
  -- **** Other conditions
  , pnull, pin, pinArr
  -- **** Unsafe condition
  , pUnsafeCond
  -- *** TabParam for child
  , TabParam(..), defTabParam
  -- *** Internals for 'UnsafeCond'
  , CondMonad, SomeToField, showCmp
  -- *** Constrainrs for Cond
  , CDBField, CDBValue, CDBFieldNullable, CDBParent, CDBChild
  -- ** Order By and others
  , OrdDirection(..), OrdFld(..), Dist(..), LO(..)
  -- *** Make OrdFld and LO
  , ordf, ascf, descf, defLO, lo1
  -- * Plain Insert
  -- ** Execute SQL
  , insertSch, insertSch_, insertText, insertText_, AllPlain
  -- ** Constraints
  , InsertReturning', InsertNonReturning'
  -- * Update
  -- ** Execute SQL
  , updateByCond, updateByCond_, updateText, updateText_
  -- ** Constraints
  , UpdateReturning, UpdateNonReturning, HListInfo, HRep
  -- * Tree-base Insert/Upsert
  -- ** Execute SQL
  , insertJSON, insertJSON_, upsertJSON, upsertJSON_, insertJSONText, insertJSONText_
  -- ** Constraints
  , InsertReturning, InsertNonReturning, UpsertReturning, UpsertNonReturning
  , SrcJSON, TgtJSON
  -- * Delete
  , deleteByCond, deleteText
  -- * Transport HList
  , HList(..), IsoHList(..), CHListInfo(..)
  -- ** Renamers
  , Renamer(..), RenamerId, CamelToSnake
  -- * Types
  , ToStar
  -- ** Tagged types
  , type (:=), (=:)
  -- ** Enum
  , PGEnum
  -- ** Aggregates
  , Aggr'(..), Aggr(..), AggrFun(..)
  -- ** Arrays
  , PgArr(..), pgArr', unPgArr'
  -- ** Conversion checks
  , CanConvert, CanConvert1, TypDef'(..)
  -- ** Other types
  , PgChar(..), PgOid(..)
  , NameNS'(..), type (->>), NameNSK
  , TRelDef, RelDef'(..), RdFrom, RdTo, FdType, FdNullable
  ) where

import PgSchema.DML.Select as S
import PgSchema.DML.Select.Types as S
import PgSchema.DML.Insert as I
import PgSchema.DML.Insert.Types as I
import PgSchema.DML.InsertJSON as I
import PgSchema.DML.Update as U
import PgSchema.DML.Delete as D
import PgSchema.HList as H
import PgSchema.Schema as S
import PgSchema.Types as T
import PgSchema.Utils.CamelToSnake
import PgSchema.Utils.Internal as T (ToStar)
