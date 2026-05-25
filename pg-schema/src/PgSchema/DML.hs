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
-- type MyAnn tabName = 'Ann CamelToSnake MySch 5 ("dbSchema" ->> tabName)
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
-- children in the same way. 'insertJSON' / 'insertJSON_' require every mandatory
-- column and emit plain @INSERT@ only (duplicates fail in the database).
-- 'upsertJSON' / 'upsertJSON_' relax mandatory requirements and may @UPDATE@ or
-- @INSERT … ON CONFLICT …@ (see their Haddock).
-- 'updateJSON' / 'updateJSON_' update existing rows only (never @INSERT@).
-- Flat 'insertSch', 'upsertByKey', and 'updateByKey' use 'PlainOut' for @RETURNING@
-- (any plain columns of the table; 'RequireBareRow' forbids @Maybe@ on the whole
-- row). Tree @*JSON@ uses 'ReturningMatches{Insert,Upsert,Update}' instead.
-- 'upsertByKey' requires mandatory columns and a full key; 'updateByKey' is
-- key-only and returns @IO ([Maybe r'], Text)@.
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
  -- * Select
    selectSch, selectText
  -- ** Query Params
  , QueryParamAnn, QueryParam(..), qpEmpty
  , qRoot, qPath, qPathFromHere, qPathToHere, qWhere, qOrderBy
  , qDistinct, qDistinctOn, qLimit, qOffset
  -- ** Conditions
  -- | Example: @"name" =? "John"@
  , (<?),(>?),(<=?),(>=?),(=?)
  -- | Null-safe equality on nullable columns (@IS NOT DISTINCT FROM@).
  -- | Example: @"suffix" =?? (Just "a")@
  , (=??)
  -- | Example: @"name" ~~? "%joh%"@
  ,(~=?),(~~?)
  , (|||), (&&&), pnot, pnull, pin, pinArr
  , pparent, pchild
  , pUnsafeCond
  -- ** Order By and Limit/Offset
  , ordf, ascf, descf, defLO, lo1
  -- ** Internals
  , Selectable, CRecInfo(..)
  , MonadQP, CondWithPath(..), OrdWithPath(..)
  , Cond(..), CondAnn, Cmp(..), BoolOp(..), TabParam(..), defTabParam
  , CondMonad, SomeToField(..), showCmp, tabPref, qual
  , CDBField, CDBValue, CDBFieldNullable, CRelDef
  , LimOffWithPath(..), DistWithPath(..)
  , OrdDirection(..), OrdFld(..), Dist(..), LO(..)
  -- * Plain DML
  -- | You can insert, update or upsert data.
  --
  -- - For insert you need all mandatory fields.
  -- - For key-based update you need a full primary or unique key.
  -- - For upsert you need all mandatory fields _and_ some key.
  --
  -- You can get in return any subset of the columns of the table which was affected by the operation.
  -- All rows returned in the same order as the input tree.
  -- For update you get `Maybe t`. It is `Nothing` if the row was not found by the key.
  --
  -- All these constraints are checked at compile time.
  --
  -- There is also operations to update or delete data by a condition.
  --
  , insertSch, insertSch_, insertText, insertText_
  , upsertByKey, upsertByKey_, upsertByKeyText, upsertByKeyText_
  , updateByKey, updateByKey_, updateByKeyText, updateByKeyText_
  , updateByCond, updateByCond_, updateText, updateText_
  , deleteByCond, deleteText
  -- ** Constraints
  , AllPlain, InsertNonReturning, InsertReturning
  , UpsertByKeyNonReturning, UpsertByKeyReturning
  , UpdateByKeyNonReturning, UpdateByKeyReturning
  , UpdateReturning, UpdateNonReturning
  -- * Tree-based DML
  -- | You can insert, update or upsert data right into the tree structure
  -- (to the several related tables at once).
  --
  -- - For insert you need all mandatory fields at every node.
  -- - For update you need a full primary or unique key at every node.
  -- - For upsert you need all mandatory fields _or_ some key at every node.
  --
  --     At each node:
  --
  --     - the operation works like an insert if there is no key
  --     - the operation works like an update if there is not all mandatory fields
  --     - the operation works like an upsert if there is all mandatory fields and a key exists
  --
  -- You don't need foreign keys for children in your data (and often even keys for parents).
  -- They will be determined automatically.
  --
  -- You can get in return any subset of the columns of the tables which were affected by the operation.
  -- All rows returned in the same order as the input tree.
  -- For update or some nodes of upsert (where not all mandatory fields are present),
  -- the returning type is `Maybe t`. It is 'Nothing' if the row was not found by the key.
  --
  -- All these constraints are checked at compile time.
  --
  -- For all these operations you should use types that can be converted to JSON (and from JSON for returning).
  -- You don't need instances but all fields should be serializable into JSON.
  -- I.e. you can't use 'Bytestring' for fields here.
  , insertJSON, insertJSON_, upsertJSON, upsertJSON_
  , updateJSON, updateJSON_, insertJSONText, insertJSONText_
  , upsertJSONText, upsertJSONText_, updateJSONText, updateJSONText_
  -- ** Constraints
  , TreeIn, TreeOut, AllMandatoryTree, AllMandatoryOrHasKeyTree, AllHasKeyTree
  , HasSchema
  , InsertTreeNonReturning, InsertTreeReturning
  , UpsertTreeNonReturning, UpsertTreeReturning
  , UpdateTreeNonReturning, UpdateTreeReturning, TRecordInfo
  , ReturningMatchesInsert, ReturningMatchesUpsert, ReturningMatchesUpdate
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
  , Aggr(..), Aggr'(..), AggrFun(..)
  -- ** Unsafe
  , UnsafeCol(..)
  -- ** Arrays
  , PgArr(..), pgArr', unPgArr'
  -- ** Conversion checks
  , CanConvert, CanConvert1, TypDef'(..)
  -- ** Other types
  , PgChar(..), PgOid(..)
  , NameNS'(..), type (->>), NameNSK, (->>)
  , PathElem'(..), PathElem, PathElemK, PathKind(..)
  , TRelDef, RelDef'(..), RdFrom, RdTo, FdType, FdNullable, CTabDef(..)
  ) where


import PgSchema.Ann
import PgSchema.DML.Select as S
import PgSchema.DML.Select.Types as S
import PgSchema.DML.Insert as I
import PgSchema.DML.Insert.Types as I
import PgSchema.DML.InsertJSON as I
import PgSchema.DML.UpsertByKey as UBK
import PgSchema.DML.Update as U
import PgSchema.DML.Delete as D
import PgSchema.Schema as S
import PgSchema.Types as T
import PgSchema.Utils.CamelToSnake
import PgSchema.Utils.Internal as T (ToStar)
