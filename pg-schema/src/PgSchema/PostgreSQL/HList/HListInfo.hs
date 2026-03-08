-- {-# OPTIONS_HADDOCK hide #-}
-- {-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE UndecidableInstances #-}
module PgSchema.PostgreSQL.HList.HListInfo
  ( CHListInfo(..), RecordInfo'(..), FieldInfo'(..)
  , RecordInfo, FieldInfo, RecordInfoK, FieldInfoK
  , RestMand, AllPlain, RestPKFlds )
  where

import Data.Kind
import Data.Text ( Text )
import PgSchema.PostgreSQL.HList.Internal
import PgSchema.PostgreSQL.HList.Type
import PgSchema.PostgreSQL.HList.Rec
  (RecordInfo'(..), FieldInfo'(..), FieldDbNameSym0, AllPlainB)
import PgSchema.Schema.Def
import PgSchema.Types.Aggr
import GHC.TypeLits as TL
import GHC.TypeError
import PgSchema.Utils
import Prelude.Singletons


-- | Value representation of 'RecordInfo''
type RecordInfo = RecordInfo' Text
-- | Type representation of 'RecordInfo''
type RecordInfoK = RecordInfo' Symbol
-- | Value representation of 'FieldInfo''
type FieldInfo = FieldInfo' Text
-- | Type representation of 'FieldInfo''
type FieldInfoK = FieldInfo' Symbol

-- | Get 'RecordInfo' to generate Sql and list of 'FieldInfoK'
-- to check constraints in compile time
class CHListInfo sch (tab :: NameNSK) r where
  type TRecordInfo sch tab r :: [FieldInfoK]
  getRecordInfo :: RecordInfo

instance (SingI tab) => CHListInfo sch tab (HList '[]) where
  type TRecordInfo sch tab (HList '[]) = '[]
  getRecordInfo = RecordInfo (demote @tab) []

data RecTypeCase = RTCAggrCount | RTCCommon

type family GetFldTypeCase t where
  GetFldTypeCase (Aggr "count" s) = RTCAggrCount
  GetFldTypeCase (Aggr' "count" s) = RTCAggrCount
  GetFldTypeCase t = RTCCommon

class CDBFieldInfoTypeCase sch (tab :: NameNSK) (fld :: (SymNat, Type)) rtc where
  type TDBFieldInfoTypeCase sch tab fld rtc :: FieldInfoK
  geTDBFieldInfo :: FieldInfo

instance (KnownSymNat '(s,n), KnownSymbol s)
  => CDBFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCAggrCount where
    type TDBFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCAggrCount =
      'FieldInfo (NameSymNat '(s,n)) s
        ('RFAggr ('FldDef ("pg_catalog" ->> "int8") False False) "count" 'True)
    geTDBFieldInfo = FieldInfo
      { fieldName = nameSymNat (s,n)
      , fieldDbName = demote @s
      , fieldKind = RFAggr (FldDef ("pg_catalog" ->> "int8") False False) "count" True }

instance (CHListInfo sch tab (HList xs), CDBFieldInfoTypeCase sch tab '(sn,t) (GetFldTypeCase t) )
  => CHListInfo sch tab (HList ('(sn,t) ': xs)) where
    type TRecordInfo sch tab (HList ('(sn,t) ': xs)) =
      TDBFieldInfoTypeCase sch tab '(sn,t) (GetFldTypeCase t)
        ': TRecordInfo sch tab (HList xs)
    getRecordInfo = ri
      { fields = geTDBFieldInfo @sch @tab @'(sn,t) @(GetFldTypeCase t) : fields ri}
      where
        ri = getRecordInfo @sch @tab @(HList xs)

instance (KnownSymNat '(s,n), KnownSymbol s
  , CTagFieldInfo sch (TDBFieldInfo sch tab s) t, CDBFieldInfo sch tab s)
  => CDBFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCCommon where
    type TDBFieldInfoTypeCase sch tab '( '(s,n),t) 'RTCCommon
      = 'FieldInfo (NameSymNat '(s,n)) s (TTagFieldInfo sch (TDBFieldInfo sch tab s) t)
    geTDBFieldInfo = FieldInfo
      { fieldName = nameSymNat (s,n)
      , fieldDbName = demote @s
      , fieldKind = demote @(TTagFieldInfo sch (TDBFieldInfo sch tab s) t) }

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

-- | Mandatory fields not included into Insert request
type RestMand sch t r rFlds =
  RestMandatory sch t (Map FieldDbNameSym0 (TRecordInfo sch t r) ++ rFlds)

-- | Used to check if Update by PK is possible
type RestPKFlds sch t r rFlds =
  RestPK sch t (Map FieldDbNameSym0  (TRecordInfo sch t r) ++ rFlds)

-- | Check that All fields are Plain, i.e. no parents or child fields included.
--
-- Used for "plain" insert/update
type family AllPlain sch tab r where
  AllPlain sch t r = Assert
    (AllPlainB (TRecordInfo sch t r))
    (TL.TypeError
      (TL.Text "Not all fields in record are 'plain' fields "
        :$$: TL.Text "Table: " :<>: TL.ShowType t
        :$$: TL.Text "Type: " :<>: TL.ShowType r
        :$$: TL.Text "Record Info: " :<>: TL.ShowType (TRecordInfo sch t r)))
