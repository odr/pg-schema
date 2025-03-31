module Database.PostgreSQL.DML.Update where

import Data.String
import Data.Text as T
import Database.PostgreSQL.DML.Select
import Database.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DB
import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util

-- TODO:
-- updateByKey Connection -> [r] -> IO [r']
-- updateByKeyJSON Connection -> [r] -> IO [r']
-- updateExp (e.q. update t set a = a + c + 1 where b > 10)

updateByCond
  :: forall r r'. forall sch t
  -> ( UpdateReturning PG sch t r r'
    , AllDmlPlain PG sch t r, ToRow r, FromRow r' )
  => Connection -> r -> Cond sch t -> IO [r']
updateByCond sch t conn r cond = returning conn q [r :. ps]
  where
    (q, ps) = updateText @r @r' sch t cond

updateByCond_ ::
  forall r. Connection -> forall sch t ->
  (CDmlRecord PG sch t r, ToRow r, AllDmlPlain PG sch t r) =>
  r -> Cond sch t -> IO Int64
updateByCond_ conn sch t r cond = do
  putStrLn q
  execute conn (fromString q) (r :. ps)
  where
    (q, ps) = updateText_ @r sch t cond

updateText
  :: forall r r' s. forall sch t
  -> (UpdateReturning  PG sch t r r', IsString s, Monoid s)
  => Cond sch t -> (s, [SomeToField])
updateText sch t  cond = (q <> " returning " <> fs', p)
  where
    (q,p) = updateText_ @r sch t cond
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromText
      $ T.intercalate "," [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']

updateText_ :: forall r s. (IsString s, Monoid s) =>
  forall sch t -> CDmlRecord PG sch t r => Cond sch t -> (s, [SomeToField])
updateText_ sch t cond =
  ("update " <> tn <> " t0 set " <> fs <> fromText whereTxt, condParams )
  where
    ur = getDmlRecord @PG @sch @t @r
    fs = intercalate' ", "
      $ [ fromText ifp.fpDbName <> " = ?" | (DmlFieldPlain ifp) <- ur.iFields]
    tn = fromText $ qualName ur.iTableName
    (condTxt, condParams) = pgCond 0 cond
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
