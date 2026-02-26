{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.Rec where

import Database.Schema.Def

import Data.Singletons.TH
import Data.String.Singletons
import Prelude.Singletons
import Text.Show.Singletons

singletons [d|
  data FieldInfo' s = FieldInfo -- p == (NameNS' s)
    { fieldName   :: s -- ~ uniq in Rec - for JSON
    , fieldDbName :: s -- for db
    , fieldKind   :: RecField' s (RecordInfo' s) }
    deriving Show

  data RecordInfo' s = RecordInfo
    { tabName :: NameNS' s
    , fields :: [FieldInfo' s] }
    deriving Show
  |]

promote [d|
  allPlainB :: [FieldInfo' s] -> Bool
  allPlainB = all (\fi -> case fieldKind fi of RFPlain _ -> True; _ -> False)
  |]
