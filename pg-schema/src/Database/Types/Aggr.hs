-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Types.Aggr where

import Data.Functor
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


aggrFldDef :: Text -> Maybe FldDef -> Maybe FldDef
aggrFldDef fname mbFD = case fname of
  "count" -> pure $ FldDef (pgc "int8") False False
  "avg" -> do mbFD <&> \fd -> fd { fdType = pgc "float8", fdNullable = True }
  "sum" -> do
    fd <- mbFD
    let t0 = nnsName $ fdType fd
    if t0 >= "int" && t0 < "inu"
      then Just fd { fdType = pgc "int8", fdNullable = True }
      else Just fd { fdType = pgc "float8", fdNullable = True }
  "min" -> mbFD <&> \fd -> fd { fdNullable = True }
  "max" -> mbFD <&> \fd -> fd { fdNullable = True }
  _ -> Nothing
