-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Types.Aggr where

import Control.Monad
import Data.Functor
import Data.List qualified as L
import Data.Map as M
import Data.Text
import Database.PostgreSQL.Schema.Catalog
import Database.Schema.Def
import Prelude.Singletons as SP
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Aeson (FromJSON, ToJSON)


newtype Aggr (fname :: Symbol) t = Aggr { unAggr :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)
-- Using enumeration for `fname` leads to extra complexity in TH (schemaRec)

aggrFldDef :: M.Map NameNS TypDef -> Text -> Maybe FldDef -> Maybe FldDef
aggrFldDef typMap fname mbFD = aggrFldDef' typMap fname mbFD <&> \fd -> case fname of
  "count" -> fd
  _ -> fd { fdNullable = True }

-- All aggregate functions except count can return NULL.
-- But if field under aggregate is mandatory they return NULL only on empty set
-- if there is no group by clause. E.g. 'select min(a) from t where false`
-- So we require Nullable for Aggr.
-- Aggr' is like Aggr but can't be used in select's without 'group by'.
-- So it is mandatory if field is mandatory.
newtype Aggr' (fname :: Symbol) t = Aggr' { unAggr' :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

aggrFldDef' :: M.Map NameNS TypDef -> Text -> Maybe FldDef -> Maybe FldDef
aggrFldDef' typMap fname mbFD = case fname of
  "count" -> pure $ FldDef (pgc "int8") False False
  "avg" -> fdCheckCat ["N"] <&> \fd -> fd { fdType = pgc "float8"}
  "sum" -> do
    fd <- fdCheckCat ["N"]
    let t0 = fd.fdType.nnsName
    if t0 >= "int" && t0 < "inu"
      then Just fd { fdType = pgc "int8" }
      else Just fd { fdType = pgc "float8" }
  "min" -> fdCheckCat ["N","S","B","D"]
  "max" -> fdCheckCat ["N","S","B","D"]
  _ -> Nothing
  where
    fdCheckCat cs = do
      fd <- mbFD
      td <- typMap !? fd.fdType
      guard $ td.typCategory `L.elem` cs
      pure fd
