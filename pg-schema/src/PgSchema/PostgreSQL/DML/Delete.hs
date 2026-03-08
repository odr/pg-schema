module PgSchema.PostgreSQL.DML.Delete where

import Data.String
import Data.Text as T
import PgSchema.PostgreSQL.DML.Select
import PgSchema.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple
import GHC.Int

import PgSchema.Schema.ShowType
import PgSchema.Utils
import Data.Singletons

-- TODO (?):
-- deleteByKey Connection -> [r] -> IO [r']

deleteByCond :: forall sch t -> SingI t =>
  Connection -> Cond sch t -> IO (Int64, (Text,[SomeToField]))
deleteByCond sch t conn cond = traceShow' (q,ps)
  $ (,(q,ps)) <$> execute conn (fromString $ T.unpack q) ps
  where
    (q, ps) = deleteText @sch @t cond

deleteText :: forall sch t s. (IsString s, Monoid s, SingI t) =>
  Cond sch t -> (s, [SomeToField])
deleteText cond =
  ("delete from " <> tn <> " t0 " <> fromText whereTxt, condParams )
  where
    tn = fromText $ qualName $ demote @t
    (condTxt, condParams) = pgCond 0 cond
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
