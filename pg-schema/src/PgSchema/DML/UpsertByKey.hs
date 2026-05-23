{-# LANGUAGE OverloadedRecordDot #-}
module PgSchema.DML.UpsertByKey
  ( upsertByKey, upsertByKey_, upsertByKeyText, upsertByKeyText_ ) where

import Control.Monad (forM)
import Data.List as L
import Data.Maybe
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import Unsafe.Coerce (unsafeCoerce)
import GHC.Int
import PgSchema.Ann
import PgSchema.DML.Insert.Types
import PgSchema.DML.KeyedWrite
import PgSchema.DML.Update (updateByKeyRowParams)
import Data.Map qualified as M
import PgSchema.Schema (CSchema, qualName, tabInfoMap)
import PgSchema.Types
import PgSchema.Utils.Internal
import Prelude as P


upsertByKey
  :: forall ann -> forall r r'. UpsertByKeyReturning ann r r'
  => Connection -> [r] -> IO ([r'], Text)
upsertByKey ann @r @r' conn recs =
  let
    sqlIns = upsertByKeyText ann @r @r'
    sqlUpd = upsertByKeyText_ ann @r <> " returning "
      <> (fromString . T.unpack . T.intercalate ",")
        [ fi.fieldDbName | fi <- (getRecordInfo @ann @r').fields ]
  in trace' (T.unpack sqlIns) do
    case keyedOp ann @r of
      KeyedUpd -> do
        rs <- forM recs \rec -> do
          rows <- query conn (fromString $ T.unpack sqlUpd)
            (updateByKeyRowParams @ann @r rec)
          pure $ case rows of
            [x] -> unPgTag @ann @r' x
            _ -> unsafeCoerce (Nothing :: Maybe ())
        pure (rs, sqlUpd)
      _ ->
        (, sqlIns)
          <$> fmap (fmap (unPgTag @ann @r'))
          ( returning conn (fromString $ T.unpack sqlIns)
          $ fmap (PgTag @ann @r) recs )

upsertByKey_
  :: forall ann -> forall r. UpsertByKeyNonReturning ann r
  => Connection -> [r] -> IO (Int64, Text)
upsertByKey_ ann @r conn recs =
  let sql = upsertByKeyText_ ann @r in
  trace' (T.unpack sql)
    $ fmap (, sql)
    . executeMany conn (fromString $ T.unpack sql)
    . fmap (PgTag @ann @r) $ recs

upsertByKeyText
  :: forall ann -> forall r r' s
  . ( CRecInfo ann r, CRecInfo ann r', IsString s, Monoid s
    , CSchema (AnnSch ann) ) => s
upsertByKeyText ann @r @r' =
  upsertByKeyText_ ann @r <> " returning " <> fs'
  where
    ri' = getRecordInfo @ann @r'
    fs' = fromText $ T.intercalate "," [fi.fieldDbName | fi <- ri'.fields]

upsertByKeyText_
  :: forall ann -> forall r s
  . (IsString s, Monoid s, CRecInfo ann r, CSchema (AnnSch ann)) => s
upsertByKeyText_ ann @r = fromString $ T.unpack $ upsertByKeyStmt ann @r

keyedOp
  :: forall ann -> forall r. (CRecInfo ann r, CSchema (AnnSch ann)) => KeyedOp
keyedOp ann @r =
  let
    ri = getRecordInfo @ann @r
    ti = tabInfoMap @(AnnSch ann) M.! ri.tabName
    srcFlds = [ fromText fi.fieldDbName | fi <- ri.fields ]
    keyNames = fromMaybe [] $ pickKeyNames ti srcFlds
  in resolveKeyedOp ti srcFlds keyNames

upsertByKeyStmt
  :: forall ann -> forall r. (CRecInfo ann r, CSchema (AnnSch ann)) => Text
upsertByKeyStmt ann @r = either id (uncurry fromMaybe) $ keyedParts ann @r

keyedParts
  :: forall ann -> forall r. (CRecInfo ann r, CSchema (AnnSch ann)) => Either Text (Text, Maybe Text)
keyedParts ann @r =
  let
    ri = getRecordInfo @ann @r
    ti = tabInfoMap @(AnnSch ann) M.! ri.tabName
    flds = [ (fromText fi.fieldDbName, "?") | fi <- ri.fields ]
    srcFlds = fst <$> flds
    keyNames = fromMaybe [] $ pickKeyNames ti srcFlds
    (plainsKey, plainsOthers) = keyedUpdateSetAndKeys ti flds
    ins =
      "insert into " <> qualName ri.tabName
      <> "(" <> intercalate' ", " srcFlds <> ") values ("
      <> intercalate' ", " (snd <$> flds) <> ")"
    upd =
      "update " <> qualName ri.tabName
      <> " set " <> intercalate' ", " (uncurry nameVal <$> plainsOthers)
      <> keyWhereClause plainsKey
    ups
      | L.null plainsOthers =
        ins <> " on conflict do nothing"
      | otherwise =
        ins <> upsertOnConflict plainsOthers keyNames
    nameVal n v = n <> " = " <> v
  in case resolveKeyedOp ti srcFlds keyNames of
    KeyedIns -> Left ins
    KeyedUpd -> Right (upd, Nothing)
    KeyedUps -> Right (upd, Just ups)
