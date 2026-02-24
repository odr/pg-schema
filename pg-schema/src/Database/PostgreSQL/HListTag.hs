module Database.PostgreSQL.HListTag
  ( HListTag(..), IsoHListTag(..)
  , streamDecodeHListTag, streamDecodeHListTag'
  ) where

import Database.PostgreSQL.HListTag.Class
import Database.PostgreSQL.HListTag.JsonStream
import Database.PostgreSQL.HListTag.Type


--------------------------------------------------------------------------------
-- 6. CHListInfo
--------------------------------------------------------------------------------

-- S.singletons [d|
--   data FieldInfo' s = FieldInfo -- p == (NameNS' s)
--     { fieldDbName :: s
--     , fieldKind   :: RecField' s (RecordInfo' s) }
--     deriving Show

--   data RecordInfo' s = RecordInfo
--     { tabName :: NameNS' s
--     , fields :: [FieldInfo' s] }
--     deriving Show
--   |]

-- type RecordInfo = RecordInfo' Text
-- type RecordInfoK = RecordInfo' Symbol

-- class CHListInfo sch tab where
--     type TRecordInfo sch tab r :: [((Symbol, RecFieldK NameNSK), Type)]
--     getRecordInfo :: RecordInfo
