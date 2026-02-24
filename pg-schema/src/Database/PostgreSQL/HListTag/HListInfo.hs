{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.HListInfo where

import Data.Kind
import Data.Singletons.TH
import Data.String.Singletons
import Data.Text as T
import Database.PostgreSQL.HListTag.Internal
import Database.PostgreSQL.HListTag.Type
import Database.Schema.Def
import Database.Types.Aggr
import Database.Types.SchList
import Prelude.Singletons
import Text.Show.Singletons


singletons [d|
  data FieldInfo' s = FieldInfo -- p == (NameNS' s)
    { fieldDbName :: s
    , fieldKind   :: RecField' s (RecordInfo' s) }
    deriving Show

  data RecordInfo' s = RecordInfo
    { tabName :: NameNS' s
    , fields :: [FieldInfo' s] }
    deriving Show
  |]

type RecordInfo = RecordInfo' Text
type RecordInfoK = RecordInfo' Symbol
type FieldInfo = FieldInfo' Text
type FieldInfoK = FieldInfo' Symbol

class CHListInfo sch tab r where
  type TRecordInfo sch tab r :: [(FieldInfoK, Type)]

instance CHListInfo sch tab (HListTag '[]) where
  type TRecordInfo sch tab (HListTag '[]) = '[]

instance CHListInfo sch (tab :: NameNSK) xs
  => CHListInfo sch tab (HListTag ('( '(s,n),t) ': xs)) where
    type TRecordInfo sch tab (HListTag ('( '(s,n),t) ': xs))
      = '( 'FieldInfo s (TTagFieldInfo sch (TFieldInfo sch tab s) t), t)
      ': TRecordInfo sch tab xs

class CTagFieldInfo sch (fi :: RecFieldK NameNSK) (t :: Type) where
  type TTagFieldInfo sch fi t :: RecFieldK RecordInfoK

instance CTagFieldInfo sch (RFPlain fd) t where
  type TTagFieldInfo sch (RFPlain fd) t = PlainField fd t

type family PlainField fd t where
  PlainField fd (Aggr fun t) = RFAggr fd fun True
  PlainField fd (Aggr' fun t) = RFAggr fd fun False
  PlainField fd t = RFPlain fd

instance CHListInfo sch fromTab t
  => CTagFieldInfo sch (RFToHere fromTab refs) (SchList t) where
    type TTagFieldInfo sch (RFToHere fromTab refs) (SchList t) = RFToHere
      ('RecordInfo fromTab (Map FstSym0 (TRecordInfo sch fromTab t))) refs

instance CHListInfo sch toTab (UnMaybe t)
  => CTagFieldInfo sch (RFFromHere toTab refs) t where
    type TTagFieldInfo sch (RFFromHere toTab refs) t = RFFromHere
      ('RecordInfo toTab (Map FstSym0 (TRecordInfo sch toTab (UnMaybe t)))) refs
