module Database.Schema.TH where

import Data.List as L
import Data.Set as S
import Data.Text as T
import Database.Schema.Def
import Database.Schema.Rec
import Language.Haskell.TH


schemaRec
  :: forall sch. CSchema sch
  => (Text -> Text) -> Name -> DecsQ
schemaRec toDbName rn = do
  TyConI (DataD _ _ _ _ [RecC _ fs] _)<- reify rn
  i1 <- L.concat <$> traverse fieldTypeInst fs
  i2 <- traverse getFieldInfo fs >>= recordInfoInst . toPromotedList
  pure $ i2 ++ i1
  where
    fieldTypeInst (n,_,t) = [d|
      instance CFieldType $(conT rn) $(pure $ nameToSym n) where
        type TFieldType $(conT rn) $(pure $ nameToSym n) = $(pure tt)
      |]
      where
        tdbname = toDbName $ pack $ nameBase n
        tt
          | tdbname `member` (rels @sch)  = case t of
            AppT ListT t' -> t'
            _             -> t
          | otherwise                     = t

    getFieldInfo (n,_,t) = [t|'FieldInfo $(kindQ) $(nameQ) $(dbNameQ)|]
      where
        tname = pack $ nameBase n
        tdbname = toDbName tname
        nameQ = pure $ nameToSym n
        dbNameQ = pure $ strToSym $ unpack $ toDbName tname
        kindQ
          | tdbname `member` (rels @sch)  = case t of
            AppT ListT _ -> [t|'FldTo|]
            _            -> [t|'FldFrom|]
          | otherwise                     = [t|'FldPlain|]
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
