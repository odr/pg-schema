{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Insert.Types where

import Database.Schema.Def
import Database.Schema.Rec
import Database.Types.SchList
import Data.Kind
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

type InsertReturning sch t r r' =
  (InsertNonReturning sch t r, CQueryRecord sch t r', SubDml sch t r r')

type InsertNonReturning sch t r =
  (CDmlRecord sch t r, AllMandatory sch t r '[])
