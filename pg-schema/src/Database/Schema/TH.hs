module Database.Schema.TH where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Functor
import Data.List as L
import Data.Map as M
import Data.Maybe as Mb
import Data.Monoid
import Data.String
import Data.Text(Text)
import qualified Data.Text as T
import Data.Traversable
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.Schema.Def
import Database.Schema.Rec
import Database.Types.Aggr
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Util.TH.LiftType


-- | Having type name and several list of type parameters type name
-- get in Q list of type and list of fields name and type.
-- Additionally we resolve all type synonyms here.
applyTypes :: Name -> [[Name]] -> Q [(Type, [(String, Type)])]
applyTypes rn (\case {[] -> [[]]; x -> x} -> npss) = do
  (fmap nt -> fs, fmap bndrType -> bndrs) <- reify rn >>= \case
    TyConI (DataD _ _ bndrs _ [RecC _ fs] _) -> pure (fs, bndrs)
    TyConI (NewtypeD _ _ bndrs _ (RecC _ fs) _) -> pure (fs, bndrs)
    x -> do
      reportError $ "schemaRec: Invalid pattern in reify: " ++ show x
      pure ([],[])
  let
    dicts = L.zipWith (\(vn,c) tn -> (vn, c tn)) bndrs <$> npss
    res = dicts <&> \ds ->
      ( L.foldl' AppT (ConT rn) $ snd <$> ds
      , fmap (applySubstitution (M.fromList ds)) <$> fs)
  traverse (traverse $ traverse $ traverse resolveTypeSynonyms) res
  where
    bndrType = \case -- TODO: investigate and make it better
      KindedTV n _ StarT -> (n, ConT)
      KindedTV n _ _ -> (n, PromotedT)
      PlainTV n _ -> (n, ConT)
    nt (n,_,t) = (nameBase n, t)

schemaRec
  :: (String -> String) -> Name -> Map NameNS TabInfo -> Map NameNS TypDef
  -> NameNS -> Name -> [[Name]] -> DecsQ
schemaRec toDbName sch tabMap typMap tab rn npss =
  applyTypes rn npss >>= fmap L.concat . traverse (schemaRec' toDbName sch tabMap typMap tab )

schemaRec'
  :: (String -> String) -> Name -> Map NameNS TabInfo -> Map NameNS TypDef
  -> NameNS -> (Type, [(String, Type)]) -> DecsQ
schemaRec' toDbName sch tabMap typMap tab (rt, fs) = do
  tinfo <- maybe (fail $ "unknown table" <> show tab) pure $ tabMap M.!? tab
  aggrC <- [t| Aggr |]
  aggrC' <- [t| Aggr' |]
  when ((Any True, Any False) == foldMap ((\t -> if
    | t == aggrC' -> (Any True, mempty)
    | t /= aggrC -> (mempty, Any True)
    | otherwise -> mempty) . snd) fs)
    $ fail "There is a record with AggrC' fields without any non-Aggr fields. It is unsafe."
  traverse (getFieldInfo aggrC aggrC' tinfo) fs >>= recordInfoInst
  where
    getFieldInfo aggrC aggrC' tinfo (sname, ft) = do
      fieldInfo <- mkFieldInfo
      (,)
        <$> [| FieldInfo (T.pack $(stringE sname)) (T.pack $(stringE $ toDbName sname))
          $ mkRecField @($(conT sch)) @($(liftType fieldInfo.fieldKind)) @($(pure ft))|]
        <*> [t| '( $(liftType fieldInfo), $(pure ft)) |]
      where
        tDbName = fromString @Text $ toDbName sname
        mkFieldInfo = do
          let mbPlainFldDef = tinfo.tiFlds M.!? tDbName
          (kind :: RecField NameNS) <- maybe
            (fail $ "can't determine kind of field '" <> sname
              <> "' for table " <> show tab)
            pure
            $ checkAggr mbPlainFldDef
              <|> fmap RFPlain mbPlainFldDef
              <|> (toFrom =<< tinfo.tiFrom M.!? (tab.nnsNamespace ->> tDbName))
              <|> (toTo . snd <=<
                L.find ((== tDbName) . (.nnsName) . fst) $ M.toList tinfo.tiTo)
          pure FieldInfo
            { fieldName   = T.pack sname
            , fieldDbName = tDbName
            , fieldKind   = kind }
          where
            checkAggr mbPlainFldDef = case ft of
              AppT (AppT ac (LitT (StrTyLit (T.pack -> fname)))) _aggrT
                | ac == aggrC -> aggrFldDef (typMap M.!?) fname mbPlainFldDef
                  <&> \fd -> RFAggr fd fname True
                | ac == aggrC' -> aggrFldDef' (typMap M.!?) fname mbPlainFldDef
                  <&> \fd -> RFAggr fd fname False
              _ -> Nothing
            toFrom rd = RFFromHere rd.rdTo <$> traverse conv rd.rdCols
              where
                conv (fromName, toName) = do
                  fromDef <- tinfo.tiFlds M.!? fromName
                  toDef <- tabMap M.!? rd.rdTo >>= (M.!? toName) . (.tiFlds)
                  pure Ref{..}
            toTo rd = RFToHere rd.rdFrom <$> traverse conv rd.rdCols
              where
                conv (fromName, toName) = do
                  toDef <- tinfo.tiFlds M.!? toName
                  fromDef <- tabMap M.!? rd.rdFrom >>= (M.!? fromName) . (.tiFlds)
                  pure Ref{..}

    recordInfoInst fis = [d|
      instance CRecordInfo $(conT sch) $(liftType tab) $(pure rt) where
        type TRecordInfo $(conT sch) $(liftType tab) $(pure rt) =
          $(pure $ toPromotedList $ snd <$> fis)
        getRecordInfo = RecordInfo tab $(pure $ ListE $ fst <$> fis)
      |]

deriveQueryRecord
  :: (String -> String) -> Name -> Map NameNS TabInfo -> Map NameNS TypDef
  -> [((Name, [[Name]]), NameNS)] -> DecsQ
deriveQueryRecord flm sch tabMap typMap = fmap L.concat . traverse \((n,nss),tab) -> do
  fss <- applyTypes n nss
  binC <- [t| Binary |]
  let
    mStr = M.fromList
      $ Mb.mapMaybe (\s -> let s' = flm s in (s,s') <$ guard (s /= s'))
      $ nub $ fst <$> foldMap snd fss
  L.concat <$> for fss \fs@(t,fields) ->
    fmap L.concat $ sequenceA $
      [ [d|deriving instance Show $(pure t)|]
      -- In JSON we need the same `fieldLabelModifier` as in 'SchemaRec'. Or not??
      , [d|instance FromRow $(pure t)|]
      , [d|instance ToRow $(pure t)|] -- for insert TODO: DELME?
      , schemaRec' flm sch tabMap typMap tab fs
      ]
      <> if jsonEnabled binC (snd <$> fields)
        then [
          [d|instance FromJSON $(pure t) where
              parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = \s -> fromMaybe s $ M.lookup s mStr } |],
          [d|instance ToJSON $(pure t) where
              toJSON = genericToJSON defaultOptions
                { fieldLabelModifier = \ss -> fromMaybe ss $ M.lookup ss mStr }
              toEncoding = genericToEncoding defaultOptions
                { fieldLabelModifier = \ss -> fromMaybe ss $ M.lookup ss mStr } |],
          [d|instance FromField $(pure t) where fromField = fromJSONField |],
          [d|instance ToField $(pure t) where toField = toJSONField |]
          ]
        else []
  where
    jsonEnabled binC = isNothing . L.find \case
      AppT t _ | t == binC -> True
      _ -> False
