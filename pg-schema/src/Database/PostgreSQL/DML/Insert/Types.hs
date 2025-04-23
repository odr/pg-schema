{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Insert.Types where

import Data.Aeson
import Data.Kind
import Data.Typeable
import Database.Schema.Def
import Database.Schema.Rec
import Database.Types.SchList
import GHC.TypeError
import GHC.TypeLits qualified as TL
import Prelude.Singletons as SP


type family AllMandatory (sch::Type) (tab::NameNSK) (r::Type) rFlds where
  AllMandatory sch t (SchList r) rFlds = AllMandatory sch t r rFlds
  AllMandatory sch t r rFlds = Assert
    (SP.Null (RestMand sch t r rFlds))
    (TL.TypeError
      ((TL.Text "Not all mandatory fields for table " :<>: TL.ShowType t
        :<>: TL.Text " in type " :<>: TL.ShowType r)
        :$$: TL.Text "Probably you have to add: " :<>: TL.ShowType (RestMand sch t r rFlds)))

type InsertReturning' sch t r r' =
  (InsertNonReturning sch t r, CRecordInfo sch t r', Typeable r')

type InsertNonReturning' sch t r =
  (CRecordInfo sch t r, AllMandatory sch t r '[], CSchema sch)

type InsertReturning sch t r r' =
  (InsertReturning' sch t r r', FromJSON r', ToJSON r)
  -- We have to check that return data only from tables in which we insert
  -- Now it is not possible because we don't store
  -- recursive RecordInfo RecordInfo on the type level...

type InsertNonReturning sch t r = (InsertNonReturning' sch t r, ToJSON r)
