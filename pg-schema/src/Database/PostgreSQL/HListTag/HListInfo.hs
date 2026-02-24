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
import GHC.TypeLits
import PgSchema.Util
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

type RecordInfo = RecordInfo' Text
type RecordInfoK = RecordInfo' Symbol
type FieldInfo = FieldInfo' Text
type FieldInfoK = FieldInfo' Symbol

class CHListInfo sch (tab :: NameNSK) r where
  type TRecordInfo sch tab r :: [FieldInfoK]
  getRecordInfo :: RecordInfo

instance (SingI tab) => CHListInfo sch tab (HListTag '[]) where
  type TRecordInfo sch tab (HListTag '[]) = '[]
  getRecordInfo = RecordInfo (demote @tab) []

instance (CHListInfo sch tab (HListTag xs), KnownSymNat '(s,n), KnownSymbol s, CTagFieldInfo sch (TFieldInfo sch tab s) t)
  => CHListInfo sch tab (HListTag ('( '(s,n),t) ': xs)) where
    type TRecordInfo sch tab (HListTag ('( '(s,n),t) ': xs))
      = 'FieldInfo (NameSymNat '(s,n)) s (TTagFieldInfo sch (TFieldInfo sch tab s) t)
      ': TRecordInfo sch tab (HListTag xs)
    getRecordInfo = RecordInfo
      { tabName = ri.tabName
      , fields = FieldInfo
        { fieldName = nameSymNat (s,n)
        , fieldDbName = demote @s
        , fieldKind = demote @(TTagFieldInfo sch (TFieldInfo sch tab s) t) }
        : ri.fields }
      where
        ri = getRecordInfo @sch @tab @(HListTag xs)

class ToStar (TTagFieldInfo sch fi t) => CTagFieldInfo sch (fi :: RecFieldK NameNSK) (t :: Type) where
  type TTagFieldInfo sch fi t :: RecFieldK RecordInfoK

instance ToStar (TTagFieldInfo sch (RFPlain fd) t) => CTagFieldInfo sch (RFPlain fd) t where
  type TTagFieldInfo sch (RFPlain fd) t = PlainField fd t

type family PlainField fd t where
  PlainField fd (Aggr fun t) = RFAggr fd fun True
  PlainField fd (Aggr' fun t) = RFAggr fd fun False
  PlainField fd t = RFPlain fd

instance (ToStar (TTagFieldInfo sch (RFToHere fromTab refs) (SchList t)), CHListInfo sch fromTab t)
  => CTagFieldInfo sch (RFToHere fromTab refs) (SchList t) where
    type TTagFieldInfo sch (RFToHere fromTab refs) (SchList t) = RFToHere
      ('RecordInfo fromTab (TRecordInfo sch fromTab t)) refs

instance (ToStar (TTagFieldInfo sch (RFFromHere toTab refs) t), CHListInfo sch toTab (UnMaybe t))
  => CTagFieldInfo sch (RFFromHere toTab refs) t where
    type TTagFieldInfo sch (RFFromHere toTab refs) t = RFFromHere
      ('RecordInfo toTab (TRecordInfo sch toTab (UnMaybe t))) refs
