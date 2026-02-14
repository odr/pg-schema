{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Insert.Types where

import Data.Aeson
import Data.Kind
import Data.Typeable
import Database.Schema.Def
import Database.Schema.Rec
import Database.Types.SchList
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.TypeError
import GHC.TypeLits qualified as TL
import Prelude.Singletons as SP


type family AllMandatory (sch::Type) (tab::NameNSK) (r::Type) rFlds where
  AllMandatory sch t (SchList r) rFlds = AllMandatory sch t r rFlds
  AllMandatory sch t r rFlds = Assert
    (SP.Null (RestMand sch t r rFlds))
    (TL.TypeError
      ( TL.Text "We can't insert data because not all mandatory fields in record."
      :$$: (TL.Text "Table: " :<>: TL.ShowType t)
      :$$: (TL.Text "Record Type: " :<>: TL.ShowType r)
      :$$: (TL.Text "To insert data you have to add fields: "
        :<>: TL.ShowType (RestMand sch t r rFlds))
      ))

-- For "plain" insert
type InsertReturning' sch t r r' = (InsertNonReturning' sch t r
  , CRecordInfo sch t r', Typeable r', FromRow r', AllPlain sch t r')

type InsertNonReturning' sch t r = (CRecordInfo sch t r
  , AllMandatory sch t r '[], CSchema sch, ToRow r, AllPlain sch t r)

-- For insertJSON
type SrcJSON sch t r = (CRecordInfo sch t r, ToJSON r, CSchema sch)

type TgtJSON sch t r' = (CRecordInfo sch t r', FromJSON r', Typeable r')
  -- We have to check that return data only from tables in which we insert
  -- Now it is not possible because we don't store
  -- recursive RecordInfo RecordInfo on the type level...

-- For Upsert:
-- AllMandatory && NoPK     => insert
-- HasPK, not AllMandatory  => update
-- HasPK, AllMandatory      => upsert
--
-- i.e. HasPK || AllMandatory

type family AllMandatoryOrHasPK (sch::Type) (tab::NameNSK) (r::Type) rFlds where
  AllMandatoryOrHasPK sch t (SchList r) rFlds = AllMandatoryOrHasPK sch t r rFlds
  AllMandatoryOrHasPK sch t r rFlds = Assert
    (SP.Null (RestMand sch t r rFlds) || SP.Null (RestPKFlds sch t r rFlds))
    (TL.TypeError
      ( TL.Text "We can't insert data because not all mandatory fields in record."
      :$$: TL.Text "We also can't update data because not all PK fields in record."
      :$$: (TL.Text "Table: " :<>: TL.ShowType t)
      :$$: (TL.Text "Record Type: " :<>: TL.ShowType r)
      :$$: (TL.Text "To insert data you have to add fields: "
        :<>: TL.ShowType (RestMand sch t r rFlds))
      :$$: (TL.Text "To update data you have to add fields: "
        :<>: TL.ShowType (RestPKFlds sch t r rFlds))
      ))

type InsertReturning sch t r r' = (InsertNonReturning sch t r, TgtJSON sch t r')

type InsertNonReturning sch t r = (SrcJSON sch t r, AllMandatory sch t r '[])

type UpsertReturning sch t r r' = (UpsertNonReturning sch t r, TgtJSON sch t r')

type UpsertNonReturning sch t r = (SrcJSON sch t r, AllMandatoryOrHasPK sch t r '[])

--------------------------------------------------------------------------------
-- CHECK BAD FIELD FOR INSERT
--------------------------------------------------------------------------------

type IsArray sch t f = IsJust (TypElem (GetTypDef sch t f))

type family IsJust (m :: Maybe a) :: Bool where
  IsJust ('Just _) = 'True
  IsJust 'Nothing  = 'False

type family CheckSafeInsArray sch t f cat isArr :: Constraint where
  CheckSafeInsArray sch t f cat 'True =
    -- Смотрим на категорию ЭЛЕМЕНТА
    If (GetElemCategory sch t f == "N" ||
        GetElemCategory sch t f == "B" ||
        GetElemCategory sch t f == "S")
       (() :: Constraint)
       (TypeError ('Text "Forbidden Insert: Field '" ':<>: 'Text f
             ':<>: 'Text "' is an array of category " ':<>: 'Text (GetElemCategory sch t f)
             ':<>: 'Text ". Only N, B, S elements are allowed in 'executeMany'."))

  CheckSafeInsArray _ _ _ _ 'False = ()

class SafeInsRow sch t (fs :: [(Symbol, Type)])
instance SafeInsRow sch t '[]

instance
  ( CFldDef sch t f
  , CheckSafeInsArray sch t f
      (GetTypCategory sch t f)
      (IsArray sch t f)
  , SafeInsRow sch t rest
  ) => SafeInsRow sch t ('(f, v) ': rest)
