{-# LANGUAGE UndecidableInstances #-}
module PgSchema.DML.Insert.Types where

import Data.Aeson
import Data.Kind
import Data.Singletons.TH
import Data.Typeable
import PgSchema.Schema
import PgSchema.HList.Class
import PgSchema.HList.HListInfo
import PgSchema.HList.Type
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.TypeError
import GHC.TypeLits qualified as TL
import Prelude.Singletons as SP


type family CheckNodeAllMandatory sch (tab :: NameNSK) (rs :: [Symbol]) :: Constraint where
  CheckNodeAllMandatory sch tab rs = Assert
    (SP.Null (RestMandatory sch tab rs))
    ( TL.TypeError
        ( TL.Text "We can't insert data because not all mandatory fields are present."
        :$$: TL.Text "Table: " :<>: TL.ShowType tab
        :$$: TL.Text "Missing mandatory fields: "
          :<>: TL.ShowType (RestMandatory sch tab rs)
        )
    )

type family CheckNodeAllMandOrPK (sch :: Type) (tab :: NameNSK) (rs :: [Symbol]) :: Constraint where
  CheckNodeAllMandOrPK sch tab rs = Assert
    (SP.Null (RestMandatory sch tab rs) || SP.Null (RestPK sch tab rs))
    ( TL.TypeError
        ( TL.Text "We can't upsert data because for table "
        :<>: TL.ShowType tab
        :$$: TL.Text "either not all mandatory fields or not all PK fields are present."
        :$$: TL.Text "Missing mandatory fields: "
          :<>: TL.ShowType (RestMandatory sch tab rs)
        :$$: TL.Text "Missing PK fields: "
          :<>: TL.ShowType (RestPK sch tab rs)
        )
    )

genDefunSymbols [''CheckNodeAllMandatory, ''CheckNodeAllMandOrPK]

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

type InsertReturning ren sch t r r' h h' = (InsertNonReturning ren sch t r h, TgtJSON ren sch t r' h')

type InsertNonReturning ren sch t r h = (SrcJSON ren sch t r h, AllMandatory sch t h '[])

type UpsertReturning ren sch t r r' h h' = (UpsertNonReturning ren sch t r h, TgtJSON ren sch t r' h')

type UpsertNonReturning ren sch t r h = (SrcJSON ren sch t r h, AllMandatoryOrHasPK sch t h '[])

type UpdateReturning ren sch t r r' h h' =
  ( h ~ HRep ren sch t r, HListInfo ren sch t r h
  , h' ~ HRep ren sch t r', HListInfo ren sch t r' h'
  , AllPlain sch t h, ToRow h, FromRow h')

type UpdateNonReturning ren sch t r h =
  (h ~ HRep ren sch t r, HListInfo ren sch t r h, ToRow h, AllPlain sch t h)

type family WalkLevel (check :: NameNSK ~> [Symbol] ~> Constraint)
  (tab :: NameNSK) (fis :: [FieldInfoK]) (rs :: [Symbol])
  :: Constraint where
  WalkLevel check tab '[] rs = SP.Apply (SP.Apply check tab) rs
  WalkLevel check tab ('FieldInfo name db ('RFPlain fd) ': xs) rs =
    WalkLevel check tab xs (name ': rs)
  WalkLevel check tab
    ('FieldInfo _ _ ('RFToHere ('RecordInfo childTab childFIs) refs) ': xs) rs =
      ( WalkLevel check childTab childFIs (SP.Map FromNameSym0 refs)
      , WalkLevel check tab xs rs )
  WalkLevel check tab (_ ': xs) rs = WalkLevel check tab xs rs

type family AllMandatory (sch :: Type) (tab :: NameNSK) (r :: Type) (rFlds :: [Symbol]) :: Constraint where
  AllMandatory sch t [r] rFlds = AllMandatory sch t r rFlds
  AllMandatory sch t r  rFlds =
    WalkLevel (CheckNodeAllMandatorySym1 sch) t (TRecordInfo sch t r) rFlds

-- For Upsert:
-- AllMandatory && NoPK     => insert
-- HasPK, not AllMandatory  => update
-- HasPK, AllMandatory      => upsert
--
-- i.e. Upsert condition: HasPK || AllMandatory
type family AllMandatoryOrHasPK (sch :: Type) (tab :: NameNSK) (r :: Type) (rFlds :: [Symbol]) :: Constraint where
  AllMandatoryOrHasPK sch t [r] rFlds = AllMandatoryOrHasPK sch t r rFlds
  AllMandatoryOrHasPK sch t r  rFlds =
    WalkLevel (CheckNodeAllMandOrPKSym1 sch) t (TRecordInfo sch t r) rFlds
