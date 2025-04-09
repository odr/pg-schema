module Database.Schema.TH where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Functor
import Data.List as L
import Data.Map as M
import Data.Maybe as Mb
import Data.String
import Data.Text as T
import Data.Traversable
import Data.Type.Bool
import Data.Type.Equality
import Database.PostgreSQL.DB
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.Schema.Def
import Database.Schema.Rec
import Database.Types.EmptyField
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Util.TH.LiftType


applyTypes :: Name -> [[Name]] -> Q [(Type, [(String, Type)])]
applyTypes rn ((\case {[] -> [[]]; x -> x}) -> npss) = do
  (fmap nt -> fs, fmap bndrType -> bndrs) <- reify rn >>= \case
    TyConI (DataD _ _ bndrs _ [RecC _ fs] _) -> pure (fs, bndrs)
    TyConI (NewtypeD _ _ bndrs _ (RecC _ fs) _) -> pure (fs, bndrs)
    x -> do
      reportError $ "schemaRec: Invalid pattern in reify: " ++ show x
      pure ([],[])
  let dicts = L.zipWith (\(vn,c) tn -> (vn, c tn)) bndrs <$> npss
  pure $ dicts <&> \ds ->
    ( L.foldr (\t ts -> AppT ts t ) (ConT rn) $ snd <$> ds
    , fmap (applySubstitution $ M.fromList ds) <$> fs)
  where
    bndrType = \case -- TODO: investigate and make it better
      KindedTV n _ StarT -> (n, ConT)
      KindedTV n _ _ -> (n, PromotedT)
      PlainTV n _ -> (n, ConT)
    nt (n,_,t) = (nameBase n, t)

schemaRec ::
  (String -> String) -> Name -> Map NameNS TabInfo -> NameNS -> Name -> [[Name]] -> DecsQ
schemaRec toDbName sch tabMap tab rn npss =
  applyTypes rn npss >>= fmap L.concat . traverse (schemaRec' toDbName sch tabMap tab )

schemaRec' ::
  (String -> String) -> Name -> Map NameNS TabInfo -> NameNS -> (Type, [(String, Type)]) -> DecsQ
schemaRec' toDbName sch tabMap tab (rt, fs) = do
  tinfo <- maybe (fail $ "unknown table" <> show tab) pure $ tabMap M.!? tab
  i1 <- L.concat <$> traverse fieldTypeInst fs
  i2 <- traverse (getFieldInfo tinfo) fs >>= recordInfoInst . toPromotedList
  pure $ i2 ++ i1
  where
    fieldTypeInst (pack -> tname, t) = [d|
      instance CFieldType $(conT sch) $(pure rt) $(liftType tname) where
        type TFieldType $(conT sch) $(pure rt) $(liftType tname) = $(pure t)
      |]

    getFieldInfo tinfo (sname, ft) = do
      fieldInfo <- mkFieldInfo
      [t| If ($(pure ft) == EmptyField)
        $(liftType $ fieldInfoEmpty) $(liftType fieldInfo) |]
      where
        tDbName = fromString @Text $ toDbName sname
        mkFieldInfo = do
          (kind :: RecField) <- maybe
            (fail $ "can't determine kind of field '" <> sname
              <> "' for table " <> show tab)
            pure
            $ fmap RFPlain ((tinfo.tiFlds :: Map Text FldDef) M.!? tDbName)
              <|> fmap RFFromHere
                (toFrom =<< tinfo.tiFrom M.!? (tab.nnsNamespace ->> tDbName))
              <|> fmap RFToHere (toTo . snd <=<
                L.find ((== tDbName) . (.nnsName) . fst) $ M.toList tinfo.tiTo)
          pure FieldInfo
            { fieldName   = T.pack sname
            , fieldDbName = tDbName
            , fieldKind   = kind }
          where
            toFrom rd = do
              recRefs <- traverse conv rd.rdCols
              pure RecRef { recRefTab = rd.rdTo, .. }
              where
                conv (fromName, toName) = do
                  fromDef <- tinfo.tiFlds M.!? fromName
                  toDef <- tabMap M.!? rd.rdTo >>= (M.!? toName) . (.tiFlds)
                  pure Ref{..}
            toTo rd = do
              recRefs <- traverse conv rd.rdCols
              pure RecRef { recRefTab = rd.rdFrom, .. }
              where
                conv (fromName, toName) = do
                  toDef <- tinfo.tiFlds M.!? toName
                  fromDef <- tabMap M.!? rd.rdFrom >>= (M.!? fromName) . (.tiFlds)
                  pure Ref{..}
        fieldInfoEmpty = FieldInfo
          { fieldName   = T.pack sname
          , fieldDbName = tDbName <> "$EmptyField"
          , fieldKind   = RFEmpty (T.pack sname) }

    recordInfoInst fis = [d|
      instance CRecordInfo $(conT sch) $(liftType tab) $(pure rt) where
        type TRecordInfo $(conT sch) $(liftType tab) $(pure rt) = $(pure fis)
      |]

deriveQueryRecord :: (String -> String) -> Name -> Map NameNS TabInfo ->
  [((Name, [[Name]]), NameNS)] -> DecsQ
deriveQueryRecord flm sch tabMap = fmap L.concat . traverse (\((n,nss),tab) -> do
  fss <- applyTypes n nss
  let
    mStr = M.fromList
      $ Mb.mapMaybe ((\s -> let s' = flm s in (s,s') <$ guard (s /= s')))
      $ nub $ fmap fst $ foldMap snd fss
  L.concat <$> for fss \fs@(t,_) ->
    L.concat <$> sequenceA
      [ [d|instance FromJSON $(pure t) where
            parseJSON = genericParseJSON defaultOptions
              { fieldLabelModifier = \s -> fromMaybe s $ M.lookup s mStr }
        |]
      , [d|instance ToJSON $(pure t) where
            toJSON = genericToJSON defaultOptions
              { fieldLabelModifier = \ss -> fromMaybe ss $ M.lookup ss mStr }
            toEncoding = genericToEncoding defaultOptions
              { fieldLabelModifier = \ss -> fromMaybe ss $ M.lookup ss mStr }
        |]
      -- , [d|deriving instance Eq $(pure t)|]
      , [d|deriving instance Show $(pure t)|]
      -- In JSON we need the same `fieldLabelModifier` as in 'SchemaRec'. Or not??
      , [d|instance FromField $(pure t) where fromField = fromJSONField |]
      , [d|instance ToField $(pure t) where toField = toJSONField |]
      , [d|instance FromRow $(pure t)|]
      , [d|instance ToRow $(pure t)|] -- for insert TODO: DELME
      , schemaRec' flm sch tabMap tab fs
      , [d|instance CQueryRecord PG $(conT sch) $(liftType tab) $(pure t)|]
      ])

deriveDmlRecord :: (String -> String) -> Name -> Map NameNS TabInfo ->
  [(Name, NameNS)] -> DecsQ
deriveDmlRecord flm sch tabMap = fmap L.concat . traverse (\(n,s) ->
  L.concat <$> sequenceA
    [ deriveJSON defaultOptions { fieldLabelModifier = flm } n
    -- In JSON we need the same `fieldLabelModifier` as in 'SchemaRec'. Or not??
    , [d|instance FromRow $(liftType n)|]
    , [d|instance ToRow $(liftType n)|]
    , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
    , [d|instance ToField $(liftType n) where toField = toJSONField |]
    , schemaRec flm sch tabMap s n []
    , [d|instance CQueryRecord PG $(conT sch) $(liftType s) $(conT n)|]
    , [d|instance CDmlRecord PG $(conT sch) $(liftType s) $(conT n)|]
    ])
