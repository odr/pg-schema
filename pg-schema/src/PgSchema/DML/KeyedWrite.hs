{-# LANGUAGE OverloadedRecordDot #-}
module PgSchema.DML.KeyedWrite
  ( mandatoryDbNames, identityCandidatesFromTab, pickKeyNames
  , keyWhereClause, upsertOnConflict
  , keyedUpdateSetAndKeys ) where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Text as T
import PgSchema.Schema
import Prelude as P


-- | Runtime conflict/update targets (same order as 'InsertJSON').
identityCandidatesFromTab :: TabInfo -> [[Text]]
identityCandidatesFromTab ti =
  P.filter (not . P.null) [ti.tiDef.tdKey] <> notNullUks <> nullUks
  where
    isNullable = (== Just True) . fmap (.fdNullable) . (`M.lookup` ti.tiFlds)
    (nullUks, notNullUks) = L.partition (P.any isNullable) ti.tiDef.tdUniq

mandatoryDbNames :: TabInfo -> [Text]
mandatoryDbNames ti =
  M.keys $ M.filter (\fd -> not $ fd.fdNullable || fd.fdHasDefault) ti.tiFlds

pickKeyNames :: TabInfo -> [Text] -> Maybe [Text]
pickKeyNames ti srcFlds =
  L.find (P.null . (L.\\ srcFlds)) $ identityCandidatesFromTab ti

keyWhereClause :: [(Text, Text)] -> Text
keyWhereClause pairs =
  " where " <> T.intercalate " and "
    (L.zipWith keyValDistinct (fst <$> pairs) (snd <$> pairs))
  where
    keyValDistinct name val = name <> " IS NOT DISTINCT FROM " <> val

-- | Partition plain columns for @UPDATE@ (SET non-keys, then WHERE keys).
keyedUpdateSetAndKeys
  :: TabInfo -> [(Text, Text)] -> ([(Text, Text)], [(Text, Text)])
keyedUpdateSetAndKeys ti plains =
  let
    srcFlds = fst <$> plains
    keyNames = fromMaybe [] $ pickKeyNames ti srcFlds
  in L.partition ((`L.elem` keyNames) . fst) plains

upsertOnConflict :: [(Text, Text)] -> [Text] -> Text
upsertOnConflict plainsOthers conflictCols =
  case (L.null plainsOthers, conflictCols) of
    (True, _) -> " on conflict do nothing"
    (_, []) -> ""
    (_, cols) ->
      " on conflict (" <> T.intercalate ", " cols <> ")"
        <> " do update set "
        <> T.intercalate ", "
          ( L.map (\(name, _) -> name <> " = EXCLUDED." <> name) plainsOthers )
