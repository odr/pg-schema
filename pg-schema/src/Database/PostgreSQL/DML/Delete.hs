module Database.PostgreSQL.DML.Delete where

import Data.String
import Data.Text as T
import Database.PostgreSQL.DML.Select
import Database.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple
import GHC.Int

import Database.Schema.ShowType
import PgSchema.Util
import Data.Singletons

-- TODO (?):
-- deleteByKey Connection -> [r] -> IO [r']

deleteByCond :: forall sch t. SingI t =>
  Connection -> Cond sch t -> IO (Int64, (Text,[SomeToField]))
deleteByCond conn cond = (,(q,ps)) <$> execute conn (fromString $ T.unpack q) ps
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
