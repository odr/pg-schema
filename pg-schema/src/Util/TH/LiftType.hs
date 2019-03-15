module Util.TH.LiftType where

import Control.Monad
import Data.List as L
import Data.Text
import Language.Haskell.TH


class LiftType a where
  liftType :: a -> TypeQ

instance LiftType Text where
  liftType = pure . txtToSym
    where
      txtToSym = LitT . StrTyLit . unpack

instance LiftType Name where
  liftType = conT

instance LiftType a => LiftType (Maybe a) where
  liftType = traverse liftType >=> maybeQ
    where
      maybeQ Nothing  = [t|'Nothing|]
      maybeQ (Just t) = appT [t|'Just|] (pure t)

instance LiftType Bool where
  liftType True  = [t|'True|]
  liftType False = [t|'False|]

instance LiftType a => LiftType [a] where
  liftType = fmap toPromotedList . traverse liftType
    where
      toPromotedList =
        L.foldr (\x xs -> AppT (AppT PromotedConsT x) xs) PromotedNilT

instance (LiftType a, LiftType b) => LiftType (a,b) where
  liftType (a,b) = [t| '( $(liftType a), $(liftType b) ) |]
