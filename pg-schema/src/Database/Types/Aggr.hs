-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Types.Aggr where

import Control.Monad
import Control.Monad.Singletons
import Data.List qualified as L
import Data.Singletons.TH
import Data.String
import Data.String.Singletons
import Database.Schema.Def
import Prelude.Singletons as SP
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Aeson (FromJSON, ToJSON)


newtype Aggr (fname :: Symbol) t = Aggr { unAggr :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)
-- Using enumeration for `fname` leads to extra complexity in TH (schemaRec)

-- All aggregate functions except count can return NULL.
-- But if field under aggregate is mandatory they return NULL only on empty set
-- if there is no group by clause. E.g. 'select min(a) from t where false`
-- So we require Nullable for Aggr.
-- Aggr' is like Aggr but can't be used in select's without 'group by'.
-- So it is mandatory if field is mandatory.
newtype Aggr' (fname :: Symbol) t = Aggr' { unAggr' :: t }
  deriving stock Show
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

promote [d|
  aggrFldDef'
    :: (Ord s, IsString s)
    => (NameNS' s -> Maybe (TypDef' s)) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDef' fTypDef fname mbFD = case fname of
    "count" -> pure $ FldDef (NameNS "pg_catalog" "int8") False False
    "avg" -> (\fd -> fd { fdType = NameNS "pg_catalog" "float8"}) <$> fdCheckCat ["N"]
    "sum" -> do
      fd <- fdCheckCat ["N"]
      let t0 = nnsName $ fdType fd
      if t0 >= "int" && t0 < "inu"
        then Just fd { fdType = NameNS "pg_catalog" "int8" }
        else Just fd { fdType = NameNS "pg_catalog" "float8" }
    "min" -> fdCheckCat ["N","S","B","D"]
    "max" -> fdCheckCat ["N","S","B","D"]
    _ -> Nothing
    where
      fdCheckCat cs = do
        fd <- mbFD
        td <- fTypDef $ fdType fd
        guard $ typCategory td `L.elem` cs
        pure fd

  aggrFldDef
    :: (Ord s, IsString s)
    => (NameNS' s -> Maybe (TypDef' s)) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDef fTypDef fname mbFD = case fname of
    "count" -> aggrFldDef' fTypDef fname mbFD
    _ -> (\fd -> fd { fdNullable = True }) <$> aggrFldDef' fTypDef fname mbFD

  aggrFldDefJ
    :: (Ord s, IsString s)
    => (NameNS' s -> TypDef' s) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDefJ f = aggrFldDef (Just . f)

  aggrFldDefJ'
    :: (Ord s, IsString s)
    => (NameNS' s -> TypDef' s) -> s -> Maybe (FldDef' s) -> Maybe (FldDef' s)
  aggrFldDefJ' f = aggrFldDef' (Just . f)
  |]
