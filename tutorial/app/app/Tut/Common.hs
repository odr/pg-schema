{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Общие определения для всех глав: ренеймер, аннотация, базовый тип.
module Tut.Common (MyRenamer, MyAnn, ProjectStatus, User(..)) where

import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import PgSchema.DML
import Sch

-- --8<-- [start:renamer]
data MyRenamer :: Renamer

type family MyRenamerImpl (s :: Symbol) :: Symbol where
  MyRenamerImpl "projectOwner" = "projects_owner_id_fkey"
  MyRenamerImpl s = CamelToSnake s

type instance ApplyRenamer MyRenamer s = MyRenamerImpl s
-- --8<-- [end:renamer]

-- --8<-- [start:ann]
type MyAnn t = 'Ann MyRenamer Sch 3 ("tut" ->> t)
-- --8<-- [end:ann]

-- --8<-- [start:project-status]
type ProjectStatus = PGEnum Sch ("tut" ->> "project_status")
-- --8<-- [end:project-status]

-- --8<-- [start:user]
data User = MkUser
  { name :: Text
  , email :: Maybe Text
  } deriving (Show, Generic)
-- --8<-- [end:user]
