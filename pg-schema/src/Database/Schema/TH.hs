module Database.Schema.TH where

import Data.List as L
import Data.Text as T
import Database.Schema.Def
import Database.Schema.Rec
import Language.Haskell.TH
import Util.TH.LiftType


schemaRec
  :: forall sch. CSchema sch
  => (Text -> Text) -> Name -> DecsQ
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
    fieldTypeInst (n,_,t) = [d|
      instance CFieldType $(conT rn) $(pure $ nameToSym n) where
        type TFieldType $(conT rn) $(pure $ nameToSym n) = $(pure t)
      |]

    getFieldInfo (n,_,_) = [t|'FieldInfo $(nameQ) $(dbNameQ)|]
      where
        tname = pack $ nameBase n
        nameQ = pure $ nameToSym n
        dbNameQ = liftType $ toDbName tname
    recordInfoInst fis = [d|
      instance CRecordInfo $(conT rn) where
        type TRecordInfo $(conT rn) = $(pure fis)
      |]

nameToSym :: Name -> Type
nameToSym = strToSym . nameBase

strToSym :: String -> Type
strToSym = LitT . StrTyLit

toPromotedList :: [Type] -> Type
toPromotedList = L.foldr (\x xs -> AppT (AppT PromotedConsT x) xs) PromotedNilT
