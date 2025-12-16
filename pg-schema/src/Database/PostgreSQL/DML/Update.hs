module Database.PostgreSQL.DML.Update where

import Data.String
import Data.Text as T
import Database.PostgreSQL.DML.Select
import Database.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple
import GHC.Int

import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util

-- TODO:
-- updateByKey Connection -> [r] -> IO [r']
-- updateByKeyJSON Connection -> [r] -> IO [r']
-- updateExp (e.q. update t set a = a + c + 1 where b > 10)

updateByCond :: forall sch t r r'.
  ( UpdateReturning sch t r r'
    , AllPlain sch t r, ToRow r, FromRow r' ) =>
  Connection -> r -> Cond sch t -> IO [r']
updateByCond conn r cond = traceShow' qps $ query conn q $ r :. ps
  where
    qps@(q, ps) = updateText @sch @t @r @r' cond

updateByCond_ :: forall sch t r.
  (CRecordInfo sch t r, ToRow r, AllPlain sch t r) =>
  Connection -> r -> Cond sch t -> IO Int64
updateByCond_ conn r cond = traceShow' qps $ execute conn q (r :. ps)
  where
    qps@(q, ps) = updateText_ @sch @t @r cond

updateText :: forall sch t r r' s.
  (UpdateReturning sch t r r', IsString s, Monoid s) =>
  Cond sch t -> (s, [SomeToField])
updateText cond = (q <> " returning " <> fs', p)
  where
    (q,p) = updateText_ @sch @t @r cond
    ri' = getRecordInfo @sch @t @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]
      -- [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']

updateText_ :: forall sch t r s. (IsString s, Monoid s, CRecordInfo sch t r) =>
  Cond sch t -> (s, [SomeToField])
updateText_ cond =
  ("update " <> tn <> " t0 set " <> fs <> fromText whereTxt, condParams )
  where
    ri = getRecordInfo @sch @t @r
    fs = intercalate' ", " [fromText fi.fieldDbName <> " = ?" | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    (condTxt, condParams) = pgCond 0 cond
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
