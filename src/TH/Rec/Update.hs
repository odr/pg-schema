module TH.Rec.Update where

import GHC.TypeLits
import Language.Haskell.TH


class SetField (fn::Symbol) r t | r fn -> t where
  setField :: t -> r -> r

mkSetField :: Name -> DecsQ
mkSetField rn = do
  TyConI (DataD _ _ _ _ [RecC _ fs] _)<- reify rn
  concat <$> mapM (\(fn,_,ft) -> mkInst fn rn ft) fs
  where
    mkInst fn r ft = do
      v <- newName "v"
      rc <- newName "rc"
      [d|
        instance SetField $(litT $ strTyLit $ nameBase fn) $(conT r) $(pure ft)
          where
            setField $(varP v) $(varP rc) =
              $(recUpdE (varE rc) [pure (fn,VarE v)])
        |]
