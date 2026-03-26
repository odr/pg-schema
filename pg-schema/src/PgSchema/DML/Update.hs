module PgSchema.DML.Update where

import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int
import PgSchema.Ann
import PgSchema.DML.Select
import PgSchema.DML.Select.Types
import PgSchema.DML.Insert.Types
import PgSchema.Schema
import PgSchema.Types
import PgSchema.Utils.Internal
import Prelude as P

-- TODO:
-- updateByKey Connection -> [r] -> IO [r']
-- updateByKeyJSON Connection -> [r] -> IO [r']
-- updateExp (e.q. update t set a = a + c + 1 where b > 10)

-- | Update rows matching a condition; the result type selects which columns are returned.
updateByCond :: forall ann -> forall r r'.
  (ann ~ 'Ann ren sch d t, UpdateReturning ann r r') =>
  Connection -> r -> Cond ren sch t -> IO [r']
updateByCond ann @r @r' conn r (updateText ann @r @r' -> (q,ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n")
  $ fmap (fmap (unPgTag @ann @r'))
  $ query conn (fromString q)
  $ PgTag @ann @r r :. ps

-- | Update records by condition without @RETURNING@.
updateByCond_ :: forall ann -> forall r.
  (ann ~ 'Ann ren sch d t, UpdateNonReturning ann r) =>
  Connection -> r -> Cond ren sch t -> IO Int64
updateByCond_ ann @r conn r (updateText_ ann @r -> (q, ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n")
  $ execute conn (fromString q)
  $ PgTag @ann @r r :. ps

-- | Construct SQL text for updating records by condition and returning some fields.
updateText :: forall ann -> forall r r' s.
  (CRecInfo ann r, CRecInfo ann r', IsString s, Monoid s, ann ~ 'Ann ren sch d t)
  => Cond ren sch t -> (s, [SomeToField])
updateText ann @r @r' (updateText_ ann @r -> (q, p)) = (q <> " returning " <> fs', p)
  where
    ri' = getRecordInfo @ann @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

-- | Construct SQL text for updating records by condition without @RETURNING@.
updateText_
  :: forall ann -> forall r s. (IsString s, Monoid s, CRecInfo ann r)
  => Cond ren sch t -> (s, [SomeToField])
updateText_ ann @r (pgCond 0 -> (condTxt, condParams)) =
  ("update " <> tn <> " t0 set " <> fs <> fromText whereTxt, condParams )
  where
    ri = getRecordInfo @ann @r
    fs = intercalate' ", " [fromText fi.fieldDbName <> " = ?" | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
