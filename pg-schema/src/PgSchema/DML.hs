-- |
-- Module: PgSchema.DML
-- Copyright: (c) Dmitry Olshansky
-- License: BSD-3-Clause
-- Maintainer: olshanskydr@gmail.com, dima@typeable.io
-- Stability: experimental
--
-- = A module to generate and safely execute SQL statements.
--
-- Among @pg-schema@’s library modules, this is the one meant for ordinary application
-- imports for database access (alongside your generated schema module).
--
-- == TLDR; Example:
--
-- @
-- data Order = Order { num :: Text, createdAt :: Day, items :: [OrdPos] } deriving Generic
-- data OrdPos = OrdPos { id :: Int32, num :: Int32, article :: Article, price :: Double } deriving Generic
-- data Article = Article { id :: Int32, name :: Text } deriving Generic
-- type MyAnn tabName = 'Ann 5 CamelToSnake MySch ("dbSchema" ->> tabName)
--     ...
-- do
--   void $ insertJSON_ (MyAnn "orders") conn
--     [ "num" =: "num1" :. "ord__ord_pos" =:
--       [ "num" =: 1 :. "articleId" =: 42 :. "price" =: 10
--       , "num" =: 2 :. "articleId" =: 41 :. "price" =: 15 ] ]
--
--   (xs :: [Order]) <- selectSch (MyAnn "orders") conn $ qRoot do
--     qWhere $ "created_at" >? someDay
--       &&& pchild "ord__ord_pos" defTabParam
--         (pparent "ord_pos__article" $ "name" ~~? "%pencil%")
--     qOrderBy [descf "created_at", descf "num"]
--     qPath "ord__ord_pos" do
--       qWhere $ pparent "ord_pos__article" $ "name" ~~? "%pencil%"
--       qOrderBy [ascf "num"]
--     qLimit 20
-- @
--
-- Here we get all orders created after `someDay` that have positions with articles like "%pencil%",
-- and return only those order items that relate to "pencil", with article info.
--
-- The preceding 'insertJSON_' call inserts one root row (here an order) and nested
-- /child/ rows (here order positions) in a single database operation, again using
-- JSON internally. Child data is carried in list fields: the field’s name (after 'Renamer') names
-- the FK constraint in the database and thus selects the child table and link;
-- each list element supplies one child row’s columns, with nested lists for further
-- children in the same way. For strict inserts,
-- 'insertJSON' and 'insertJSON_' require every mandatory column at each node;
-- 'upsertJSON' / 'upsertJSON_' relax that so each row can be resolved by keys and
-- optional columns (see their Haddock). Plain 'insertSch' / 'insertSch_' follow the
-- same safety story for flat (non-tree) rows.
--
-- 'selectSch' decodes each row into a Haskell type @r@ whose fields describe the
-- root table columns and nested data for relations (multiple list-shaped children
-- and nested parents/'Maybe' as appropriate). The type drives the generated SQL
-- shape; monadic 'QueryParam' only adds what this call needs—filters, ordering on a
-- branch, limits, and so on.
--
-- We can use both 'GHC.Generics.Generic'-based and 'PgTag'-based ('(=:)') interfaces and mix them in any way.
--
-- All string literals at the type level are 'GHC.TypeLits.Symbol' thanks to @RequiredTypeArguments@.
-- Operations are safe when the live database schema matches the generated schema types.
--
-- The @INSERT@ and @SELECT@ shown here each run as a single, fast database round-trip (JSON inside PostgreSQL).
--
-- We use a 'Renamer' (e.g. 'CamelToSnake') to map Haskell field names to @snake_case@ in the database.
-- In conditions and 'QueryParams', column names are the original database names.
-- It can be changed in the future.
--
module PgSchema.DML
  (
  -- * DML
  -- ** Select
  -- *** Execute SQL
    selectSch, selectText, Selectable
  -- *** Monad to set Query Params
  , MonadQP, qpEmpty
  , qRoot, qPath, qWhere, qOrderBy, qDistinct, qDistinctOn, qLimit, qOffset
  -- **** Internals
  , QueryParam(..), CondWithPath(..), OrdWithPath(..), LimOffWithPath(..), DistWithPath(..)
  -- *** Conditions
  -- | Example: @"name" =? "John"@
  , (<?),(>?),(<=?),(>=?),(=?)
  -- | Example: @"name" ~~? "%joh%"@
  ,(~=?),(~~?)
  , (|||), (&&&), pnot, pnull, pin, pinArr
  , pparent, pchild, TabParam(..), defTabParam
  , pUnsafeCond, UnsafeCol(..)
  , Cond(..), Cmp(..), BoolOp(..)
  , CondMonad, SomeToField(..), showCmp, tabPref, qual
  , CDBField, CDBValue, CDBFieldNullable, CRelDef
  -- *** Order By and others
  , ordf, ascf, descf, defLO, lo1
  , OrdDirection(..), OrdFld(..), Dist(..), LO(..)
  -- ** Plain Insert
  , insertSch, insertSch_, insertText, insertText_, AllPlain
  , InsertNonReturning, InsertReturning
  -- ** Update
  , updateByCond, updateByCond_, updateText, updateText_
  , UpdateReturning, UpdateNonReturning, CRecInfo(..)
  -- ** Tree-base Insert/Upsert
  , insertJSON, insertJSON_, upsertJSON, upsertJSON_, insertJSONText, insertJSONText_
  , TreeIn, TreeOut, AllMandatoryTree, AllMandatoryOrHasPKTree, TreeSch
  , InsertTreeNonReturning, InsertTreeReturning
  , UpsertTreeNonReturning, UpsertTreeReturning, TRecordInfo
  -- ** Delete
  , deleteByCond, deleteText
  -- * Types
  , Ann(..), ToStar
  -- ** Renamers
  , RenamerId, CamelToSnake, Renamer, ApplyRenamer, ApplyRenamerNS
  -- ** PgTag types
  , type (:=), (=:), PgTag(..),
  -- | Re-export from postgresql-simple
  (:.)(..)
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
  , NameNS'(..), type (->>), NameNSK, (->>)
  , TRelDef, RelDef'(..), RdFrom, RdTo, FdType, FdNullable, CTabDef(..)
  ) where


import PgSchema.Ann
import PgSchema.DML.Select as S
import PgSchema.DML.Select.Types as S
import PgSchema.DML.Insert as I
import PgSchema.DML.Insert.Types as I
import PgSchema.DML.InsertJSON as I
import PgSchema.DML.Update as U
import PgSchema.DML.Delete as D
import PgSchema.Schema as S
import PgSchema.Types as T
import PgSchema.Utils.CamelToSnake
import PgSchema.Utils.Internal as T (ToStar)
