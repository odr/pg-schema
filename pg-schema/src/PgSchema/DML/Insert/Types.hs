{-# LANGUAGE UndecidableInstances #-}
module PgSchema.DML.Insert.Types where

import Data.Aeson
import Data.Kind
import Data.Typeable
import PgSchema.Schema
import PgSchema.HList
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.TypeError
import GHC.TypeLits qualified as TL
import Prelude.Singletons as SP


type family AllMandatory (sch::Type) (tab::NameNSK) (r::Type) rFlds where
  AllMandatory sch t [r] rFlds = AllMandatory sch t r rFlds -- ???
  AllMandatory sch t r rFlds = Assert
    (SP.Null (RestMand sch t r rFlds))
    (TL.TypeError
      ( TL.Text "We can't insert data because not all mandatory fields in record."
      :$$: (TL.Text "Table: " :<>: TL.ShowType t)
      :$$: (TL.Text "Record Type: " :<>: TL.ShowType r)
      :$$: (TL.Text "TRecordInfo: " :<>: TL.ShowType (TRecordInfo sch t r))
      :$$: (TL.Text "To insert data you have to add fields: "
        :<>: TL.ShowType (RestMand sch t r rFlds))
      ))

type HListInfo ren sch t r h = ( IsoHList ren sch t r, CHListInfo sch t h )
type HRep ren sch t r = HList (HListRep ren sch t r)

-- For "plain" insert
type InsertReturning' ren sch t r r' h h' =
  ( InsertNonReturning' ren sch t r h, h' ~ HRep ren sch t r'
  , HListInfo ren sch t r' h', Typeable h', FromRow h', AllPlain sch t h')

type InsertNonReturning' ren sch t r h =
  ( h ~ HRep ren sch t r, HListInfo ren sch t r h
  , AllMandatory sch t h '[], CSchema sch, ToRow h, AllPlain sch t h)

-- For insertJSON
type SrcJSON ren sch t r h =
  ( IsoHList ren sch t r, h ~ HList (HListRep ren sch t r)
  , CHListInfo sch t h, ToJSON h, CSchema sch, Typeable (HListRep ren sch t r))

type TgtJSON ren sch t r' h' =
  ( h' ~ HRep ren sch t r', HListInfo ren sch t r' h', FromJSON h', Typeable h')
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
  AllMandatoryOrHasPK sch t [r] rFlds = AllMandatoryOrHasPK sch t r rFlds
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

type InsertReturning ren sch t r r' h h' = (InsertNonReturning ren sch t r h, TgtJSON ren sch t r' h')

type InsertNonReturning ren sch t r h = (SrcJSON ren sch t r h, AllMandatory sch t h '[])

type UpsertReturning ren sch t r r' h h' = (UpsertNonReturning ren sch t r h, TgtJSON ren sch t r' h')

type UpsertNonReturning ren sch t r h = (SrcJSON ren sch t r h, AllMandatoryOrHasPK sch t h '[])

type UpdateReturning ren sch t r r' h h' =
  ( h ~ HRep ren sch t r, HListInfo ren sch t r h
  , h' ~ HRep ren sch t r', HListInfo ren sch t r' h' )
