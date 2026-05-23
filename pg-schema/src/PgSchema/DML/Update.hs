{-# LANGUAGE AllowAmbiguousTypes #-}
module PgSchema.DML.Update
  ( updateByKey, updateByKey_, updateByKeyText, updateByKeyText_
  , updateByCond, updateByCond_, updateText, updateText_
  , updateByKeyRowParams ) where

import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import GHC.Int
import PgSchema.Ann
import PgSchema.DML.Select
import PgSchema.DML.Select.Types
import Control.Monad (forM)
import Data.List as L
import Data.Maybe
import PgSchema.DML.Insert.Types
import PgSchema.DML.KeyedWrite
import Data.Map qualified as M
import PgSchema.Schema (CSchema, qualName, tabInfoMap)
import PgSchema.Types
import PgSchema.Utils.Internal
import Prelude as P

newtype KeyedUpdateParams = KeyedUpdateParams { keyedUpdateActions :: [Action] }

instance ToRow KeyedUpdateParams where
  toRow = keyedUpdateActions

updateByKeyRowParams
  :: forall ann r
  . (CRecInfo ann r, CSchema (AnnSch ann), ToRow (PgTag ann r))
  => r -> KeyedUpdateParams
updateByKeyRowParams rec =
  let
    ri = getRecordInfo @ann @r
    ti = tabInfoMap @(AnnSch ann) M.! ri.tabName
    keyNames = fromMaybe [] $ pickKeyNames ti [fi.fieldDbName | fi <- ri.fields]
    (fldKeys, fldOthers) =
      L.partition ((`L.elem` keyNames) . (.fieldDbName)) ri.fields
    allActs = toRow (PgTag @ann @r rec)
    pick fi =
      allActs
        !! fromMaybe (error "updateByKeyRowParams: field")
          (L.findIndex ((== fi.fieldDbName) . (.fieldDbName)) ri.fields)
  in KeyedUpdateParams $ P.map pick (fldOthers ++ fldKeys)

-- | Update rows by primary / unique key from record fields (never inserts).
--
-- Returning type @r'@ is a bare row; the result is @IO ([Maybe r'], Text)@
-- with @Nothing@ when no row matched the key.
updateByKey
  :: forall ann -> forall r r'. UpdateByKeyReturning ann r r'
  => Connection -> [r] -> IO ([Maybe r'], Text)
updateByKey ann @r @r' conn recs =
  let sql = updateByKeyText ann @r @r' in
  trace' (T.unpack sql) do
    rs <- forM recs \rec -> do
      rows <- query conn (fromString $ T.unpack sql) (updateByKeyRowParams @ann @r rec)
      pure $ case rows of
        [x] -> Just (unPgTag @ann @r' x)
        _ -> Nothing
    pure (rs, sql)

-- | Update rows by key without @RETURNING@.
updateByKey_
  :: forall ann -> forall r. UpdateByKeyNonReturning ann r
  => Connection -> [r] -> IO (Int64, Text)
updateByKey_ ann @r conn recs =
  let sql = updateByKeyText_ ann @r in
  trace' (T.unpack sql) do
    n <- executeMany conn (fromString $ T.unpack sql)
      $ fmap (updateByKeyRowParams @ann @r) recs
    pure (n, sql)

updateByKeyText
  :: forall ann -> forall r r' s
  . ( CRecInfo ann r, CRecInfo ann r', IsString s, Monoid s, HasSchema ann ) => s
updateByKeyText ann @r @r' =
  updateByKeyText_ ann @r <> " returning " <> fs'
  where
    ri' = getRecordInfo @ann @r'
    fs' = fromString $ T.unpack $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

updateByKeyText_
  :: forall ann -> forall r s
  . (IsString s, Monoid s, CRecInfo ann r, HasSchema ann) => s
updateByKeyText_ ann @r = fromString $ T.unpack $ updateByKeyStmt ann @r

updateByKeyStmt
  :: forall ann -> forall r. (CRecInfo ann r, HasSchema ann) => T.Text
updateByKeyStmt ann @r =
  let
    ri = getRecordInfo @ann @r
    ti = tabInfoMap @(AnnSch ann) M.! ri.tabName
    plains = [ (fromText fi.fieldDbName, "?") | fi <- ri.fields ]
    (plainsKey, plainsOthers) = keyedUpdateSetAndKeys ti plains
    nameVal n v = n <> " = " <> v
    setClause =
      case plainsOthers of
        [] -> case plainsKey of
          (n, _) : _ -> n <> " = " <> n
          [] -> "id = id"
        xs -> T.intercalate ", " (uncurry nameVal <$> xs)
  in "update " <> qualName ri.tabName
    <> " set " <> setClause
    <> keyWhereClause plainsKey

-- | Update rows matching a condition; the result type selects which columns are returned.
updateByCond :: forall ann -> forall r r'.
  (UpdateReturning ann r r') => Connection -> r -> CondAnn ann -> IO [r']
updateByCond ann @r @r' conn r (updateText ann @r @r' -> (q,ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n")
  $ fmap (fmap (unPgTag @ann @r'))
  $ query conn (fromString q)
  $ PgTag @ann @r r :. ps

-- | Update records by condition without @RETURNING@.
updateByCond_ :: forall ann -> forall r. UpdateNonReturning ann r =>
  Connection -> r -> CondAnn ann -> IO Int64
updateByCond_ ann @r conn r (updateText_ ann @r -> (q, ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n")
  $ execute conn (fromString q)
  $ PgTag @ann @r r :. ps

-- | Construct SQL text for updating records by condition and returning some fields.
updateText :: forall ann -> forall r r' s.
  (CRecInfo ann r, CRecInfo ann r', IsString s, Monoid s)
  => CondAnn ann -> (s, [SomeToField])
updateText ann @r @r' (updateText_ ann @r -> (q, p)) = (q <> " returning " <> fs', p)
  where
    ri' = getRecordInfo @ann @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

-- | Construct SQL text for updating records by condition without @RETURNING@.
updateText_
  :: forall ann -> forall r s. (IsString s, Monoid s, CRecInfo ann r)
  => CondAnn ann -> (s, [SomeToField])
updateText_ ann @r (pgCond 0 -> (condTxt, condParams)) =
  ("update " <> tn <> " t0 set " <> fs <> fromText whereTxt, condParams )
  where
    ri = getRecordInfo @ann @r
    fs = intercalate' ", " [fromText fi.fieldDbName <> " = ?" | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
