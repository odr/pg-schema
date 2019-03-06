{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.Schema.Join where

import Data.Singletons.Prelude as SP
import Data.Singletons.TH
import Data.Text (Text)
import Database.Schema.Def
import GHC.TypeLits
import Util.ToStar


singletons [d|

  data JoinType =
    JoinLeft | JoinRight | JoinFull | JoinInner deriving (Eq, Show)

  data JoinDef' s = JoinDef
    { jType :: JoinType
    , jFrom :: s
    , prefixFrom :: s
    , jRel :: s
    , prefixTo :: s }
    deriving Show

  data CrossJoinDef' s = CrossJoinDef
    { tab1 :: s
    , prefix1 :: s
    , tab2 :: s
    , prefix2 :: s }
    deriving Show

  joinTabDef :: TabDef' s -> (s -> s) -> TabDef' s -> (s -> s) -> TabDef' s
  joinTabDef (TabDef fs1 ks1 uks1) pref1 (TabDef fs2 _ _) pref2 = TabDef
    { tdFlds = map pref1 fs1 ++ map pref2 fs2
    , tdKey  = map pref1 ks1
    , tdUniq = map (map pref1) uks1 }

  -- joinRelDef :: RelDef' s ->
  |]

promote [d|

  joinFldDef :: Bool -> Bool -> JoinType -> FldDef' s -> FldDef' s
  joinFldDef isRelFld isFrom joinType fd
    | (isFrom && joinType == JoinRight)
      || (not isFrom && joinType == JoinLeft)
      || (joinType == JoinFull && isRelFld)
      = fd { fdNullable = True }
    | (isFrom && joinType == JoinLeft && isRelFld)
      || (not isFrom && joinType == JoinRight && isRelFld)
      || (joinType == JoinFull && isRelFld)
      = fd { fdNullable = True }
    | otherwise = fd
  |]

type JoinDefK = JoinDef' Symbol
type JoinDef = JoinDef' Text

genDefunSymbols [''AppendSymbol]

class
  ( ToStar name, ToStar (TJoinDef sch name), ToStar (JTabDef sch name)
  , TTabDef sch (JFrom (TJoinDef sch name))
    ~ TTabDef sch (RdFrom (TRelDef sch (JRel (TJoinDef sch name))))
  , ToStar (AppSym1 (PrefixFrom (TJoinDef sch name)))
  , ToStar (AppSym1 (PrefixTo (TJoinDef sch name))) )
  => CJoinDef sch (name :: Symbol) where
  type TJoinDef sch name :: JoinDefK

  type JTabDef sch name :: TabDefK
  type JTabDef sch name = JoinTabDef
    (JFromTabDef sch name)
    (AppSym1 (PrefixFrom (TJoinDef sch name)))
    (JToTabDef sch name)
    (AppSym1 (PrefixTo (TJoinDef sch name)))

type JFromTabDef sch name = TTabDef sch (JFrom (TJoinDef sch name))
type AppSym1 p = AppendSymbolSym1 (AppendSymbol p "_")
type JToTabDef sch name =
  TTabDef sch (RdTo (TRelDef sch (JRel (TJoinDef sch name))))

-- instance CJoinDef j name => CTabDef sch name where
--   type TTabDef sch name = JTabDef sch name
