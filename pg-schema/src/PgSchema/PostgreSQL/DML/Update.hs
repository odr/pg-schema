module PgSchema.PostgreSQL.DML.Update where

import Data.String
import Data.Text as T
import PgSchema.PostgreSQL.DML.Select
import PgSchema.PostgreSQL.DML.Select.Types
import PgSchema.PostgreSQL.DML.Insert.Types
import PgSchema.PostgreSQL.HList
import Database.PostgreSQL.Simple
import GHC.Int

import PgSchema.Schema.ShowType
import PgSchema.Utils
import Prelude as P

-- TODO:
-- updateByKey Connection -> [r] -> IO [r']
-- updateByKeyJSON Connection -> [r] -> IO [r']
-- updateExp (e.q. update t set a = a + c + 1 where b > 10)

updateByCond :: forall ren sch t -> forall r r' h h'.
  (UpdateReturning ren sch t r r' h h', AllPlain sch t h, ToRow h, FromRow h') =>
  Connection -> r -> Cond sch t -> IO [r']
updateByCond ren sch t @_r @_r' @h @h' conn r (updateText sch t @h @h' -> (q,ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n")
  $ fmap (fmap (fromHList @ren @sch @t))
  $ query conn (fromString q)
  $ toHList @ren @sch @t r :. ps

updateByCond_ :: forall ren sch t -> forall r h.
  (h ~ HRep ren sch t r, HListInfo ren sch t r h, ToRow h, AllPlain sch t h) =>
  Connection -> r -> Cond sch t -> IO Int64
updateByCond_ ren sch t @_r @h conn r (updateText_ sch t @h -> (q, ps)) =
  trace' (q <> "\n\n" <> P.show ps <> "\n\n")
  $ execute conn (fromString q)
  $ toHList @ren @sch @t r :. ps

updateText :: forall sch t -> forall r r' s.
  (CHListInfo sch t r, CHListInfo sch t r', IsString s, Monoid s) =>
  Cond sch t -> (s, [SomeToField])
updateText sch t @r @r' (updateText_ sch t @r -> (q, p)) = (q <> " returning " <> fs', p)
  where
    ri' = getRecordInfo @sch @t @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

updateText_
  :: forall sch t -> forall r s. (IsString s, Monoid s, CHListInfo sch t r)
  => Cond sch t -> (s, [SomeToField])
updateText_ sch t @r (pgCond 0 -> (condTxt, condParams)) =
  ("update " <> tn <> " t0 set " <> fs <> fromText whereTxt, condParams )
  where
    ri = getRecordInfo @sch @t @r
    fs = intercalate' ", " [fromText fi.fieldDbName <> " = ?" | fi <- ri.fields]
    tn = fromText $ qualName ri.tabName
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
