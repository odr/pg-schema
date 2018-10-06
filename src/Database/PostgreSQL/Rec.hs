{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.PostgreSQL.Rec where

import Data.Kind
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Text as T
import Database.PostgreSQL.Convert
import Database.Schema.Def
import Database.Schema.Util.ToStar


promote [d|
  data FldRecInfo s = FldRecInfoC
    { friName     :: s
    , friPgName   :: s
    , friFldDef   :: FldDef s }

  data RefRecInfo s = RefRecInfoC
    { rriName     :: s
    , rriRefTab   :: s
    , rriFromFlds :: [s]
    , rriToFlds   :: [s] }

  data FldRecInfoSum s
    = FldPlain (FldRecInfo s)
    | FldTo (RefRecInfo s)
    | FldFrom (RefRecInfo s)

  data FldRecDef s t = FldRecDefC
    { frdInfo :: FldRecInfoSum s
    , frdType :: t }
  |]

type instance TStar (FldRecInfo Symbol) = FldRecInfo Text
type instance TStar (RefRecInfo Symbol) = RefRecInfo Text
type instance TStar (FldRecInfoSum Symbol) = FldRecInfoSum Text

instance (ToStar a, ToStar b, ToStar c)
  => ToStar ('FldRecInfoC a b c :: FldRecInfo Symbol) where
    toStar = FldRecInfoC (toStar @_ @a) (toStar @_ @b) (toStar @_ @c)

instance (ToStar a, ToStar b, ToStar c, ToStar d)
  => ToStar ('RefRecInfoC a b c d :: RefRecInfo Symbol) where
    toStar =
      RefRecInfoC (toStar @_ @a) (toStar @_ @b) (toStar @_ @c) (toStar @_ @d)

instance ToStar a => ToStar ('FldPlain a :: FldRecInfoSum Symbol) where
    toStar = FldPlain (toStar @_ @a)

instance ToStar a => ToStar ('FldTo a :: FldRecInfoSum Symbol) where
    toStar = FldTo (toStar @_ @a)

instance ToStar a => ToStar ('FldFrom a :: FldRecInfoSum Symbol) where
    toStar = FldFrom (toStar @_ @a)


type family CanFldConvert sch (frd :: FldRecDef Symbol Type) :: Constraint where
  CanFldConvert sch ('FldRecDefC ('FldPlain fri) t) =
    ( CanConvert sch (FdType (FriFldDef fri)) (FdNullable (FriFldDef fri)) t
    , ToStar fri )
  CanFldConvert sch ('FldRecDefC ('FldTo rri) t) =
    CRecDef sch (RdTo (TRelDef sch (RriName rri))) t
  CanFldConvert sch ('FldRecDefC ('FldFrom rri) t) =
    CRecDef sch (RdFrom (TRelDef sch (RriName rri))) t

type family CanRecConvert sch (frd::[FldRecDef Symbol Type]) :: Constraint where
  CanRecConvert sch '[] = ()
  CanRecConvert sch (x ': xs) = (CanFldConvert sch x, CanRecConvert sch xs)

class
  ( CanRecConvert sch (TRecDef sch tab a)
  , ToStar (RecDefInfo sch tab a)
  , ToStar tab )
  => CRecDef (sch::Type) (tab :: Symbol) (a::Type) where
    type TRecDef sch tab a   :: [FldRecDef Symbol Type]

type RecDefInfo sch tab a = Map FrdInfoSym0 (TRecDef sch tab a)
