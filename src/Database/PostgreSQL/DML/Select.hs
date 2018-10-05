-- {-# LANGUAGE CPP #-}
module Database.PostgreSQL.DML.Select where

import Data.Semigroup ((<>))
import Data.Text as T
import Database.PostgreSQL.Rec
import Database.Schema.Util.ToStar

select :: forall sch tab rec. CRecDef sch tab rec => Text
select = sel
  where
    sel = "select "
      <> T.intercalate "," (friName <$> toStar @_ @(RecFldInfos sch tab rec))
      <> " from " <> toStar @_ @tab
