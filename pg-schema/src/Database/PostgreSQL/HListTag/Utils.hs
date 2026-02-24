{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.Utils where

import Control.Applicative
import Data.Kind (Type)
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.HListTag.Internal
import Database.PostgreSQL.HListTag.Type
import GHC.TypeLits


-- | Generic initialization of an 'HListTag' lifted into an 'Alternative'
-- functor @f@ (e.g. @Maybe@). All slots are set to 'empty'.
class HEmpty ts f where
  hEmpty :: HListTag (Lifted ts f)

instance Alternative f => HEmpty '[] f where
  hEmpty = HNil

instance (Alternative f, HEmpty ts f) => HEmpty ('(s, t) ': ts) f where
  hEmpty = PgTag empty :* hEmpty @ts @f

-- | Lifting values into an Applicative/Functor structure
class HLift (ts :: [(SymNat, Type)]) where
  hlift :: (forall x. x -> f x)
        -> HListTag ts
        -> HListTag (Lifted ts f)

type family Lifted (ts :: [(SymNat, Type)]) (f :: Type -> Type) :: [(SymNat, Type)] where
  Lifted '[] f = '[]
  Lifted ('(s, t) ': ts) f = '(s, f t) ': Lifted ts f

instance HLift '[] where
  hlift _ HNil = HNil

instance HLift ts => HLift ('(s, t) ': ts) where
  hlift f (PgTag x :* xs) = PgTag (f x) :* hlift f xs

-- | Rank-2 \"sequence\" for 'HListTag': collapse an extra 'Applicative'
-- layer around all fields.
class HUnLift ts f where
  hUnLift :: Applicative f => HListTag (Lifted ts f) -> f (HListTag ts)

instance Applicative f => HUnLift '[] f where
  hUnLift HNil = pure HNil

instance (Applicative f, HUnLift ts f) => HUnLift ('(s, t) ': ts) f where
  hUnLift (PgTag ft :* rest) =
    (:*) . PgTag <$> ft <*> hUnLift rest

class HListAppend (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) where
  appendHListTag :: HListTag xs -> HListTag ys -> HListTag (AppendHList xs ys)

instance HListAppend '[] ys where
  appendHListTag HNil ys = ys

instance HListAppend xs ys => HListAppend (x ': xs) ys where
  appendHListTag (x :* xs) ys = x :* appendHListTag xs ys

class NormalizeGoHListTag (prefix :: [SymNat]) (xs :: [(SymNat, Type)]) where
  normalizeGoHListTag :: HListTag xs -> HListTag (NormalizeGo prefix xs)

instance NormalizeGoHListTag prefix '[] where
  normalizeGoHListTag HNil = HNil

instance
  ( NormalizeGoHListTag (sn ': prefix) rest
  , KnownSymNat sn s n
  , s ~ SymNatSymbol sn
  , n' ~ CountSymInKeys prefix s
  , KnownNat n'
  )
  => NormalizeGoHListTag prefix ('(sn, t) ': rest)
  where
  normalizeGoHListTag (PgTag x :* rest) =
    PgTag @'(s, n') x :* normalizeGoHListTag @(sn ': prefix) @rest rest

class NormalizeHListTag (xs :: [(SymNat, Type)]) where
  normalizeHListTag :: HListTag xs -> HListTag (Normalize xs)

instance (out ~ NormalizeGo '[] xs, NormalizeGoHListTag '[] xs) => NormalizeHListTag xs where
  normalizeHListTag = normalizeGoHListTag @'[] @xs

class SplitAtHListTag (xs :: [(SymNat, Type)]) (ys :: [(SymNat, Type)]) where
  splitAtHListTag :: HListTag (AppendHList xs ys) -> (HListTag xs, HListTag ys)

instance SplitAtHListTag '[] ys where
  splitAtHListTag h = (HNil, h)

instance SplitAtHListTag rest ys => SplitAtHListTag ('(sn, t) ': rest) ys where
  splitAtHListTag (PgTag x :* tl) =
    let (restH, ysH) = splitAtHListTag tl
    in (PgTag x :* restH, ysH)

class SplitAtHListTagN (n :: Nat) (zs :: [(SymNat, Type)]) where
  splitAtHListTagN :: HListTag zs -> (HListTag (Take n zs), HListTag (Drop n zs))

instance SplitAtHListTagN 0 zs where
  splitAtHListTagN h = (HNil, h)

instance
  ( m ~ (n - 1)
  , SplitAtHListTagN m rest
  , Take n ('(sn, t) ': rest) ~ ('(sn, t) ': Take m rest)
  , Drop n ('(sn, t) ': rest) ~ Drop m rest
  )
  => SplitAtHListTagN n ('(sn, t) ': rest)
  where
  splitAtHListTagN (PgTag x :* tl) =
    let (a, b) = splitAtHListTagN @m @rest tl
    in (PgTag x :* a, b)

class RetagHListTag (a :: [(SymNat, Type)]) (b :: [(SymNat, Type)]) where
  retagHListTag :: HListTag a -> HListTag b

instance RetagHListTag '[] '[] where
  retagHListTag HNil = HNil

instance (RetagHListTag resta restb, TypesOf a ~ TypesOf b) =>
  RetagHListTag ('(sna, t) ': resta) ('(snb, t) ': restb)
  where
  retagHListTag (PgTag x :* rest) = PgTag @snb x :* retagHListTag rest
