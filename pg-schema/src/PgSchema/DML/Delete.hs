module PgSchema.DML.Delete where

import Data.String
import Data.Text as T
import PgSchema.DML.Select
import PgSchema.DML.Select.Types
import Database.PostgreSQL.Simple
import GHC.Int

import PgSchema.Schema
import PgSchema.Utils.Internal
import Data.Singletons


-- | Delete records in table by condition.
--
deleteByCond :: forall ren sch t -> SingI t =>
  Connection -> Cond ren sch t -> IO (Int64, (Text,[SomeToField]))
deleteByCond ren sch t conn cond = traceShow' (q,ps)
  $ (,(q,ps)) <$> execute conn (fromString $ T.unpack q) ps
  where
    (q, ps) = deleteText @ren @sch @t cond

-- | Construct SQL text for deleting records by condition.
--
deleteText :: forall ren sch t s. (IsString s, Monoid s, SingI t) =>
  Cond ren sch t -> (s, [SomeToField])
deleteText cond =
  ("delete from " <> tn <> " t0 " <> fromText whereTxt, condParams )
  where
    tn = fromText $ qualName $ demote @t
    (condTxt, condParams) = pgCond 0 cond
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
