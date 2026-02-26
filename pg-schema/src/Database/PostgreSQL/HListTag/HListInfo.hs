{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.HListInfo
  ( CHListInfo(..), RecordInfo'(..), FieldInfo'(..)
  , RecordInfo, FieldInfo, RecordInfoK, FieldInfoK
  , RestMand, AllPlain, RestPKFlds, allPlainB )
  where

import Data.Kind
import Data.Text as T ( Text )
import Database.PostgreSQL.HListTag.Internal
import Database.PostgreSQL.HListTag.Type
import Database.PostgreSQL.HListTag.Rec
import Database.Schema.Def
import Database.Types.Aggr
import GHC.TypeLits as TL
import GHC.TypeError
import PgSchema.Util
import Prelude.Singletons


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

data RecTypeCase = RTCAggrCount | RTCCommon

type family GetFldTypeCase t where
  GetFldTypeCase (Aggr "count" s) = RTCAggrCount
  GetFldTypeCase (Aggr' "count" s) = RTCAggrCount
  GetFldTypeCase t = RTCCommon

class CFieldInfoTypeCase sch (tab :: NameNSK) (fld :: (SymNat, Type)) rtc where
  type TFieldInfoTypeCase sch tab fld rtc :: FieldInfoK
  getFieldInfo :: FieldInfo

instance (KnownSymNat '(s,n), KnownSymbol s)
  => CFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCAggrCount where
    type TFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCAggrCount =
      'FieldInfo (NameSymNat '(s,n)) s
        ('RFAggr ('FldDef ("pg_catalog" ->> "int8") False False) "count" 'True)
    getFieldInfo = FieldInfo
      { fieldName = nameSymNat (s,n)
      , fieldDbName = demote @s
      , fieldKind = RFAggr (FldDef ("pg_catalog" ->> "int8") False False) "count" True }

instance (CHListInfo sch tab (HListTag xs), CFieldInfoTypeCase sch tab '(sn,t) (GetFldTypeCase t) )
  => CHListInfo sch tab (HListTag ('(sn,t) ': xs)) where
    type TRecordInfo sch tab (HListTag ('(sn,t) ': xs)) =
      TFieldInfoTypeCase sch tab '(sn,t) (GetFldTypeCase t)
        ': TRecordInfo sch tab (HListTag xs)
    getRecordInfo = ri
      { fields = getFieldInfo @sch @tab @'(sn,t) @(GetFldTypeCase t) : fields ri}
      where
        ri = getRecordInfo @sch @tab @(HListTag xs)

instance (KnownSymNat '(s,n), KnownSymbol s
  , CTagFieldInfo sch (TFieldInfo sch tab s) t, CFieldInfo sch tab s)
  => CFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCCommon where
    type TFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCCommon
      = 'FieldInfo (NameSymNat '(s,n)) s (TTagFieldInfo sch (TFieldInfo sch tab s) t)
    getFieldInfo = FieldInfo
      { fieldName = nameSymNat (s,n)
      , fieldDbName = demote @s
      , fieldKind = demote @(TTagFieldInfo sch (TFieldInfo sch tab s) t) }

class ToStar (TTagFieldInfo sch fi t) => CTagFieldInfo sch (fi :: RecFieldK NameNSK) (t :: Type) where
  type TTagFieldInfo sch fi t :: RecFieldK RecordInfoK

instance ToStar (TTagFieldInfo sch (RFPlain fd) t) => CTagFieldInfo sch (RFPlain fd) t where
  type TTagFieldInfo sch (RFPlain fd) t = PlainField fd t

type family PlainField fd t where
  PlainField fd (Aggr fun t) = RFAggr fd fun True
  PlainField fd (Aggr' fun t) = RFAggr fd fun False
  PlainField fd t = RFPlain fd

instance (ToStar (TTagFieldInfo sch (RFToHere fromTab refs) [t]), CHListInfo sch fromTab t)
  => CTagFieldInfo sch (RFToHere fromTab refs) [t] where
    type TTagFieldInfo sch (RFToHere fromTab refs) [t] = RFToHere
      ('RecordInfo fromTab (TRecordInfo sch fromTab t)) refs

instance (ToStar (TTagFieldInfo sch (RFFromHere toTab refs) t), CHListInfo sch toTab (UnMaybe t))
  => CTagFieldInfo sch (RFFromHere toTab refs) t where
    type TTagFieldInfo sch (RFFromHere toTab refs) t = RFFromHere
      ('RecordInfo toTab (TRecordInfo sch toTab (UnMaybe t))) refs

type RestMand sch t r rFlds =
  RestMandatory sch t (Map FieldDbNameSym0 (TRecordInfo sch t r) ++ rFlds)

type RestPKFlds sch t r rFlds =
  RestPK sch t (Map FieldDbNameSym0  (TRecordInfo sch t r) ++ rFlds)


type family AllPlain sch tab r where
  AllPlain sch t r = Assert
    (AllPlainB (TRecordInfo sch t r))
    (TL.TypeError
      (TL.Text "Not all fields in record are 'plain' fields "
        :$$: TL.Text "Table: " :<>: TL.ShowType t
        :$$: TL.Text "Type: " :<>: TL.ShowType r
        :$$: TL.Text "Record Info: " :<>: TL.ShowType (TRecordInfo sch t r)))
