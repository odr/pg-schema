{-# LANGUAGE UndecidableInstances #-}
module PgSchema.HList.Utils where

import Control.Applicative
import Data.Kind (Type)
import PgSchema.HList.Internal
import PgSchema.HList.Type
import PgSchema.Schema
import Prelude.Singletons


-- | Generic initialization of an 'HList' lifted into an 'Alternative'
-- functor @f@ (e.g. @Maybe@). All slots are set to 'empty'.
class HEmpty ts f where
  hEmpty :: HList (Lifted ts f)

instance Alternative f => HEmpty '[] f where
  hEmpty = HNil

instance (Alternative f, HEmpty ts f) => HEmpty ('(s, t) ': ts) f where
  hEmpty = empty :* hEmpty @ts @f

-- | Lifting values into an Applicative/Functor structure
class HLift (ts :: [(SymNat, Type)]) where
  hlift :: (forall x. x -> f x)
        -> HList ts
        -> HList (Lifted ts f)

type family Lifted (ts :: [(SymNat, Type)]) (f :: Type -> Type) :: [(SymNat, Type)] where
  Lifted '[] f = '[]
  Lifted ('(s, t) ': ts) f = '(s, f t) ': Lifted ts f

instance HLift '[] where
  hlift _ HNil = HNil

instance HLift ts => HLift ('(s, t) ': ts) where
  hlift f (x :* xs) = f x :* hlift f xs

-- | Rank-2 \"sequence\" for 'HList': collapse an extra 'Applicative'
-- layer around all fields.
class HUnLift ts f where
  hUnLift :: Applicative f => HList (Lifted ts f) -> f (HList ts)

instance Applicative f => HUnLift '[] f where
  hUnLift HNil = pure HNil

instance (Applicative f, HUnLift ts f) => HUnLift ('(s, t) ': ts) f where
  hUnLift (ft :* rest) = (:*) <$> ft <*> hUnLift rest

class HListAppend (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) where
  appendHList :: HList xs -> HList ys -> HList (xs ++ ys)

instance HListAppend '[] ys where
  appendHList HNil ys = ys

instance HListAppend xs ys => HListAppend (x ': xs) ys where
  appendHList (x :* xs) ys = x :* appendHList xs ys

type NormalizeHList xs = RetagHList xs (Normalize xs)

normalizeHList :: RetagHList xs (Normalize xs) => HList xs -> HList (Normalize xs)
denormalizeHList :: RetagHList xs (Normalize xs) => HList (Normalize xs) -> HList xs
normalizeHList = retagHList
denormalizeHList = reretagHList

class SplitAtHList (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) where
  splitAtHList :: HList (xs ++ ys) -> (HList xs, HList ys)

instance SplitAtHList '[] ys where
  splitAtHList h = (HNil, h)

instance SplitAtHList rest ys => SplitAtHList ('(sn, t) ': rest) ys where
  splitAtHList (x :* tl) =
    let (restH, ysH) = splitAtHList tl in (x :* restH, ysH)

class RetagHList (a :: [(SymNat, Type)]) (b :: [(SymNat, Type)]) where
  retagHList :: HList a -> HList b
  reretagHList :: HList b -> HList a

instance RetagHList '[] '[] where
  retagHList HNil = HNil
  reretagHList HNil = HNil

instance (RetagHList as bs) => RetagHList ('(sa, t) ': as) ('(sb, t) ': bs) where
  retagHList (x :* xs) = x :* retagHList xs
  reretagHList (x :* xs) = x :* reretagHList xs
