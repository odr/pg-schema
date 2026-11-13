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
-- == Example
--
-- @
-- data MyRenamer :: Renamer
--
-- type family MyRenamerImpl (s :: Symbol) :: Symbol where
--   MyRenamerImpl "items"   = "ord\_\_ord_pos"     -- FK from ord_pos to ord
--   MyRenamerImpl "article" = "ord_pos\_\_article" -- FK from ord_pos to article
--   MyRenamerImpl s         = CamelToSnake s
--
-- type instance ApplyRenamer MyRenamer s = MyRenamerImpl s
--
-- type MyAnn tabName = 'Ann MyRenamer MySch 5 ("dbSchema" ->> tabName)
--
-- data Order = Order { num :: Text, createdAt :: Day, items :: [OrdPos] } deriving Generic
-- data OrdPos = OrdPos { id :: Int32, num :: Int32, article :: Article, price :: Double } deriving Generic
-- data Article = Article { id :: Int32, name :: Text } deriving Generic
--     ...
-- do
--   void $ insertJSON_ (MyAnn "ord") conn
--     [ "num" =: "num1" :. "items" =:
--       [ "num" =: 1 :. "articleId" =: 42 :. "price" =: 10
--       , "num" =: 2 :. "articleId" =: 41 :. "price" =: 15 ] ]
--
--   (xs :: [Order]) <- fst \<$\> selectSch (MyAnn "ord") conn (qRoot do
--     qWhere $ "created_at" >? someDay
--       &&& pchild "items" defTabParam
--         (pparent "article" $ "name" ~~? "%pencil%")
--     qOrderBy [descf "created_at", descf "num"]
--     qPath "items" do
--       qWhere $ pparent "article" $ "name" ~~? "%pencil%"
--       qOrderBy [ascf "num"]
--     qLimit 20)
-- @
--
-- Here we get all orders created after @someDay@ that have positions with articles like @"%pencil%"@,
-- and return only those order items that relate to "pencil", with article info.
--
-- The preceding 'insertJSON_' call inserts one root row (here an order) and nested
-- /child/ rows (here order positions) in a single database operation, using
-- JSON internally. Child data is carried in list fields: the field’s name (after the
-- 'Renamer') names the FK constraint in the database and thus selects the child table
-- and link; each list element supplies one child row’s columns, with nested lists for
-- further children in the same way.
--
-- * 'insertJSON' / 'insertJSON_' require every mandatory column and emit plain
--   @INSERT@ only (duplicates fail in the database).
-- * 'upsertJSON' / 'upsertJSON_' relax mandatory requirements and may @UPDATE@ or
--   @INSERT … ON CONFLICT …@ (see 'upsertJSON').
-- * 'updateJSON' / 'updateJSON_' update existing rows only (never @INSERT@).
--
-- Flat (single-table) 'insertSch', 'upsertByKey' and 'updateByKey' may return any
-- plain columns of the table; the returning row itself must not be wrapped in
-- 'Maybe'. Tree @*JSON@ operations check the returning type with
-- 'ReturningMatchesInsert', 'ReturningMatchesUpsert' and 'ReturningMatchesUpdate'
-- instead. 'upsertByKey' requires all mandatory columns and a full key;
-- 'updateByKey' requires only a full key and returns @IO ([Maybe r'], Text)@.
--
-- 'selectSch' decodes each row into a Haskell type @r@ whose fields describe the
-- root table columns and nested data for relations (multiple list-shaped children
-- and nested parents/'Maybe' as appropriate). The type drives the generated SQL
-- shape; monadic t'QueryParam' only adds what this call needs: filters, ordering on a
-- branch, limits, and so on.
--
-- We can use both 'GHC.Generics.Generic'-based and t'PgTag'-based ('(=:)') interfaces and mix them in any way.
--
-- All string literals at the type level are 'GHC.TypeLits.Symbol' thanks to @RequiredTypeArguments@.
-- Operations are safe when the live database schema matches the generated schema types.
--
-- The @INSERT@ and @SELECT@ shown here each run as a single, fast database round-trip (JSON inside PostgreSQL).
--
-- A 'Renamer' maps Haskell field names to database names. It is a type-level
-- function declared as an empty data type with an 'ApplyRenamer' instance (see
-- 'ApplyRenamer' and the example above); 'CamelToSnake' converts @camelCase@ to
-- @snake_case@ and is a convenient default case for your renamer.
-- The renamer is applied to all names: record fields, column names in conditions
-- and ordering, relation names in 'qPath' / 'pchild' / 'pparent'. So with a renamer
-- based on 'CamelToSnake' you can write either @"ownerId"@ or @"owner_id"@.
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
  -- Example: @"suffix" =?? (Just "a")@
  , (=??)
  -- | @LIKE@ ('~=?') and @ILIKE@ ('~~?'). Example: @"name" ~~? "%joh%"@
  ,(~=?),(~~?)
  , (|||), (&&&), pnot, pnull, pin, pinArr
  , pparent, pchild
  , pUnsafeCond
  -- ** Order By and Limit/Offset
  , ordf, ascf, descf, defLO, lo1
  -- ** Internals
  -- | Types and functions that appear in the signatures above, and the building
  -- blocks for unsafe conditions and ordering ('pUnsafeCond', 'UnsafeOrd').
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
  -- - For upsert you need all mandatory fields /and/ some key.
  --
  -- You can get in return any subset of the columns of the table which was affected by the operation.
  -- For key-based update each returned row is @Maybe r'@: it is 'Nothing' if the row was not found by the key.
  --
  -- All these constraints are checked at compile time.
  --
  -- There are also operations to update or delete data by a condition.
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
  -- - For upsert you need all mandatory fields /or/ some key at every node.
  --
  --     At each node:
  --
  --     - the operation works like an insert if there is no key;
  --     - the operation works like an update if not all mandatory fields are present;
  --     - the operation works like an upsert if all mandatory fields and a key are present.
  --
  -- You don't need foreign keys for children in your data (and often even keys for parents).
  -- They will be determined automatically.
  --
  -- You can get in return any subset of the columns of the tables which were affected by the operation.
  -- All rows are returned in the same order as in the input tree.
  -- For update or some nodes of upsert (where not all mandatory fields are present),
  -- the returning type is @Maybe r'@. It is 'Nothing' if the row was not found by the key.
  --
  -- All these constraints are checked at compile time.
  --
  -- For all these operations you should use types that can be converted to JSON (and from JSON for returning).
  -- You don't need instances but all fields should be serializable into JSON.
  -- E.g. you can't use 'Data.ByteString.ByteString' fields here.
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
  , type (:=), (=:), PgTag(..), named, Named,
  -- ** Plain values of rows
  rowsOf, rowOf, plains, plain, Plain, PlainRow,
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
  , CanConvert, CanConvert1, CheckAggr, TypDef'(..)
  -- ** Other types
  , PgChar(..)
  , NameNS'(..), type (->>), NameNSK, (->>)
  , PathElem'(..), PathElem, PathElemK, PathKind(..)
  , TRelDef, RelDef'(..), FldDef', CTabDef(..)
  -- | Type-level selectors (promoted record fields): 'RdFrom' and 'RdTo' give
  -- the child and the parent table of a t'RelDef'', 'FdType' and 'FdNullable'
  -- give the type and the nullability of a t'FldDef''.
  , RdFrom, RdTo, FdType, FdNullable
  ) where


import PgSchema.Ann
import PgSchema.Plain
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
