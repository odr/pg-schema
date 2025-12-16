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
import Prelude as P

-- TODO:
-- updateByKey Connection -> [r] -> IO [r']
-- updateByKeyJSON Connection -> [r] -> IO [r']
-- updateExp (e.q. update t set a = a + c + 1 where b > 10)

updateByCond :: forall sch t r r'.
  (UpdateReturning sch t r r', AllPlain sch t r, ToRow r, FromRow r') =>
  Connection -> r -> Cond sch t -> IO [r']
updateByCond conn r (updateText @sch @t @r @r' -> (q,ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n") $ query conn (fromString q) $ r :. ps

updateByCond_ :: forall sch t r.
  (CRecordInfo sch t r, ToRow r, AllPlain sch t r) =>
  Connection -> r -> Cond sch t -> IO Int64
updateByCond_ conn r (updateText_ @sch @t @r -> (q, ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n") $ execute conn (fromString q) (r :. ps)

updateText :: forall sch t r r' s.
  (UpdateReturning sch t r r', IsString s, Monoid s) =>
  Cond sch t -> (s, [SomeToField])
updateText (updateText_ @sch @t @r -> (q, p)) = (q <> " returning " <> fs', p)
  where
    ri' = getRecordInfo @sch @t @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

updateText_ :: forall sch t r s. (IsString s, Monoid s, CRecordInfo sch t r) =>
  Cond sch t -> (s, [SomeToField])
updateText_ (pgCond 0 -> (condTxt, condParams)) =
  ("update " <> tn <> " t0 set " <> fs <> fromText whereTxt, condParams )
  where
    ri = getRecordInfo @sch @t @r
    fs = intercalate' ", " [fromText fi.fieldDbName <> " = ?" | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
