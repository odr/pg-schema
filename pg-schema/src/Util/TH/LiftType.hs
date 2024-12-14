module Util.TH.LiftType where

import Control.Monad
import Data.List as L
import Data.Text
import Language.Haskell.TH


class LiftType a where
  liftType :: a -> TypeQ

instance LiftType Text where
  liftType = pure . LitT . StrTyLit . unpack

instance LiftType Name where
  liftType = conT

instance LiftType a => LiftType (Maybe a) where
  liftType = traverse liftType >=> maybe [t|'Nothing|] (appT [t|'Just|] . pure)

instance LiftType Bool where
  liftType True  = [t|'True|]
  liftType False = [t|'False|]

instance LiftType a => LiftType [a] where
  liftType = fmap toPromotedList . traverse liftType

toPromotedList :: [Type] -> Type
toPromotedList = L.foldr (AppT . AppT PromotedConsT) PromotedNilT

instance (LiftType a, LiftType b) => LiftType (a,b) where
  liftType (a,b) = [t| '( $(liftType a), $(liftType b) ) |]
