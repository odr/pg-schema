module PgSchema.PostgreSQL.HList
  (
  -- * Base transport type
    T.HList(..)
  -- * IsoHList
  -- | Converting user data to tranport 'HList'
  , C.IsoHList(..), C.Renamer(..), C.RenamerId
  -- * HListInfo
  -- | Meta-information about 'HList' to generate SQL and compile-time checking
  , I.CHListInfo(..)
  -- | 'RecordInfo'' as a table name and list of 'FieldInfo''
  , I.RecordInfo'(..), I.RecordInfo, I.RecordInfoK
  -- | FieldInfo' as a
  --
  -- - unique (in the record) field name
  --
  -- - database name (field or reference constraint)
  --
  -- - kind of field ('RecField'')
  --
  , I.FieldInfo'(..), I.FieldInfo, I.FieldInfoK
  -- * Utility Type Families
  -- | Some type families over 'TRecordInfo'
  , I.AllPlain
  , I.RestMand
  , I.RestPKFlds
  ) where

import PgSchema.PostgreSQL.HList.Class qualified as C
import PgSchema.PostgreSQL.HList.Type qualified as T
import PgSchema.PostgreSQL.HList.HListInfo qualified as I
