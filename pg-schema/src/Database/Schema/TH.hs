module Database.Schema.TH where

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

schemaRec :: (String -> String) -> Name -> [[Name]] -> DecsQ
schemaRec toDbName rn npss =
  applyTypes rn npss >>= fmap L.concat . traverse (schemaRec' toDbName)

schemaRec' :: (String -> String) -> (Type, [(String, Type)]) -> DecsQ
schemaRec' toDbName = mkInstances
  where
    mkInstances (rt,fs) = do
      i1 <- L.concat <$> traverse (fieldTypeInst rt) fs
      i2 <- traverse getFieldInfo fs >>= recordInfoInst rt . toPromotedList
      pure $ i2 ++ i1

    fieldTypeInst rt (pack -> tname, t) = [d|
      instance CFieldType $(pure rt) $(liftType tname) where
        type TFieldType $(pure rt) $(liftType tname) = $(pure t)
      |]

    getFieldInfo (sname, ft) =
      [t|'FieldInfo $(liftType $ T.pack sname)
        (If ($(pure ft) == EmptyField)
          $(liftType $ tDbNameE) $(liftType tDbName))|]
      where
        tDbName = fromString @Text $ toDbName sname
        tDbNameE = tDbName <> "$EmptyField"

    recordInfoInst rt fis = [d|
      instance CRecordInfo $(pure rt) where
        type TRecordInfo $(pure rt) = $(pure fis)
      |]

deriveQueryRecord
  :: (String -> String) -> Name -> [((Name, [[Name]]), NameNS)] -> DecsQ
deriveQueryRecord flm sch = fmap L.concat . traverse (\((n,nss),s) -> do
  fss <- applyTypes n nss
  let
    mStr = M.fromList
      $ Mb.mapMaybe ((\ss -> let s' = flm ss in (ss,s') <$ guard (ss /= s')))
      $ nub $ fmap fst $ foldMap snd fss
  L.concat <$> for fss \fs@(t,_) ->
    L.concat <$> sequenceA
      [ [d|instance FromJSON $(pure t) where
            parseJSON = genericParseJSON defaultOptions
              { fieldLabelModifier = \ss -> fromMaybe ss $ M.lookup ss mStr }
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
      , schemaRec' flm fs
      , [d|instance CQueryRecord PG $(conT sch) $(liftType s) $(pure t)|]
      ])

deriveDmlRecord
  :: (String -> String) -> Name -> [(Name, NameNS)] -> DecsQ
deriveDmlRecord flm sch = fmap L.concat . traverse (\(n,s) ->
  L.concat <$> sequenceA
    [ deriveJSON defaultOptions { fieldLabelModifier = flm } n
    -- In JSON we need the same `fieldLabelModifier` as in 'SchemaRec'. Or not??
    , [d|instance FromRow $(liftType n)|]
    , [d|instance ToRow $(liftType n)|]
    , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
    , [d|instance ToField $(liftType n) where toField = toJSONField |]
    , schemaRec flm n []
    , [d|instance CQueryRecord PG $(conT sch) $(liftType s) $(conT n)|]
    , [d|instance CDmlRecord PG $(conT sch) $(liftType s) $(conT n)|]
    ])
