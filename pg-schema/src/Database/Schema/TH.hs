module Database.Schema.TH where

import Data.Aeson.TH
import Data.List as L
import Data.String
import Data.Text as T
import Database.PostgreSQL.DB
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.Schema.Def
import Database.Schema.Rec
import Language.Haskell.TH
import Util.TH.LiftType


schemaRec :: (String -> String) -> Name -> DecsQ
schemaRec toDbName rn = do
  fs <- reify rn >>= \case
    TyConI (DataD _ _ _ _ [RecC _ fs] _) -> pure fs
    TyConI (NewtypeD _ _ _ _ (RecC _ fs) _) -> pure fs
    x -> do
      reportError $ "schemaRec: Invalid pattern in reify: " ++ show x
      pure []

  i1 <- L.concat <$> traverse fieldTypeInst fs
  i2 <- traverse getFieldInfo fs >>= recordInfoInst . toPromotedList
  pure $ i2 ++ i1
  where
    fieldTypeInst (pack . nameBase -> tname,_,t) = [d|
      instance CFieldType $(liftType rn) $(liftType tname) where
        type TFieldType $(liftType rn) $(liftType tname) = $(pure t)
      |]

    getFieldInfo (nameBase -> sname, _, _) =
      [t|'FieldInfo $(liftType $ T.pack sname) $(liftType tDbName)|]
      where
        tDbName = fromString @Text $ toDbName sname

    recordInfoInst fis = [d|
      instance CRecordInfo $(liftType rn) where
        type TRecordInfo $(liftType rn) = $(pure fis)
      |]

deriveQueryRecord
  :: (String -> String) -> Name -> [(Name, NameNS)] -> DecsQ
deriveQueryRecord flm sch = fmap L.concat . traverse (\(n,s) ->
  L.concat <$> sequenceA
    [ deriveJSON defaultOptions { fieldLabelModifier = flm } n
    -- In JSON we need the same `fieldLabelModifier` as in 'SchemaRec'. Or not??
    , [d|instance FromRow $(liftType n)|]
    -- , [d|instance ToRow $(liftType n)|] -- for insert TODO: DELME
    , [d|instance FromField $(liftType n) where fromField = fromJSONField |]
    -- , [d|instance ToField $(liftType n) where toField = toJSONField |]
    , schemaRec flm n
    , [d|instance CQueryRecord PG $(conT sch) $(liftType s) $(conT n)|]
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
    , schemaRec flm n
    , [d|instance CQueryRecord PG $(conT sch) $(liftType s) $(conT n)|]
    , [d|instance CInsertRecord PG $(conT sch) $(liftType s) $(conT n)|]
    ])
