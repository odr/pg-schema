module Database.Schema.TH where

import Data.List as L
import Data.Map as M
import Data.Text as T
import Database.Schema.Def
import Database.Schema.Rec
import Language.Haskell.TH


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

  schList <- [t|SchList|]
  i1 <- L.concat <$> traverse (fieldTypeInst schList) fs
  i2 <- traverse (getFieldInfo schList) fs >>= recordInfoInst . toPromotedList
  pure $ i2 ++ i1
  where
    fieldTypeInst schList (n,_,t) = [d|
      instance CFieldType $(conT rn) $(pure $ nameToSym n) where
        type TFieldType $(conT rn) $(pure $ nameToSym n) = $(pure tt)
      |]
      where
        tdbname = toDbName $ pack $ nameBase n
        tt
          | tdbname `member` (relDefMap @sch) = case t of
            AppT con t' | con == schList -> t'
            _                            -> t
          | otherwise = t

    getFieldInfo schList (n,_,t) = [t|'FieldInfo $(kindQ) $(nameQ) $(dbNameQ)|]
      where
        tname = pack $ nameBase n
        tdbname = toDbName tname
        nameQ = pure $ nameToSym n
        dbNameQ = pure $ txtToSym $ toDbName tname
        kindQ
          | tdbname `member` (relDefMap @sch) = case t of
            AppT con _ | con == schList -> [t|'FldTo|]
            _                           -> [t|'FldFrom|]
          | otherwise = [t|'FldPlain|]
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

txtToSym :: Text -> Type
txtToSym = strToSym . unpack
