module PgSchema.DML.Delete where

import Data.String
import Data.Text as T
import PgSchema.DML.Select
import PgSchema.DML.Select.Types
import Database.PostgreSQL.Simple
import GHC.Int

import PgSchema.Ann
import PgSchema.Schema
import PgSchema.Utils.Internal
import Data.Singletons


-- | Delete records in table by condition.
--
deleteByCond :: forall ann -> SingI (AnnTab ann) => Connection -> CondAnn ann
  -> IO (Int64, (Text,[SomeToField]))
deleteByCond ann conn cond = traceShow' (q,ps)
  $ (,(q,ps)) <$> execute conn (fromString $ T.unpack q) ps
  where
    (q, ps) = deleteText @ann cond

-- | Construct SQL text for deleting records by condition.
--
deleteText :: forall ann s. (IsString s, Monoid s, SingI (AnnTab ann)) =>
  CondAnn ann -> (s, [SomeToField])
deleteText cond =
  ("delete from " <> tn <> " t0 " <> fromText whereTxt, condParams )
  where
    tn = fromText $ qualName $ demote @(AnnTab ann)
    (condTxt, condParams) = pgCond 0 cond
    whereTxt
      | T.null condTxt = mempty
      | otherwise = " where " <> condTxt
