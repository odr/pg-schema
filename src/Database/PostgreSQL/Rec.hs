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

  data FldRecDef s t = FldRecDefC
    { frdInfo     :: FldRecInfo s
    , frdType     :: t }

  data RefRecInfo s = RefRecInfoC
    { rriName     :: s
    , rriRefTab   :: s
    , rriFromFlds :: [s]
    , rriToFlds   :: [s] }

  data RefRecDef s t = RefRecDefC
    { rrdInfo     :: RefRecInfo s
    , rrdType     :: t  -- for child recs it is a type of list elements
    }
  |]

type instance TStar (FldRecInfo Symbol) = FldRecInfo Text
type instance TStar (RefRecInfo Symbol) = RefRecInfo Text

instance (ToStar a, ToStar b, ToStar c)
  => ToStar ('FldRecInfoC a b c :: FldRecInfo Symbol) where
    toStar = FldRecInfoC (toStar @_ @a) (toStar @_ @b) (toStar @_ @c)

instance (ToStar a, ToStar b, ToStar c, ToStar d)
  => ToStar ('RefRecInfoC a b c d :: RefRecInfo Symbol) where
    toStar =
      RefRecInfoC (toStar @_ @a) (toStar @_ @b) (toStar @_ @c) (toStar @_ @d)

type FrdPgType frd = FdType (FriFldDef (FrdInfo frd))
type FrdNullable frd = FdNullable (FriFldDef (FrdInfo frd))

type CanFldConvert sch frd =
  ( CanConvert sch (FrdPgType frd) (FrdNullable frd) (FrdType frd)
  , ToStar (FrdInfo frd) )

type family CanRecConvert sch (frd::[FldRecDef Symbol Type]) :: Constraint where
  CanRecConvert sch '[] = ()
  CanRecConvert sch (x ': xs) = (CanFldConvert sch x, CanRecConvert sch xs)

type CFromDef sch rrd =
  CRecDef sch (RdFrom (TRelDef sch (RriName (RrdInfo rrd)))) (RrdType rrd)

type CToDef sch rrd =
  CRecDef sch (RdTo (TRelDef sch (RriName (RrdInfo rrd)))) (RrdType rrd)

type family CanFromConvert sch (rrd :: [RefRecDef Symbol Type]) :: Constraint
  where
    CanFromConvert sch '[] = ()
    CanFromConvert sch (x ': xs) = (CFromDef sch x, CanFromConvert sch xs)

type family CanToConvert sch (rrd :: [RefRecDef Symbol Type]) :: Constraint
  where
    CanToConvert sch '[] = ()
    CanToConvert sch (x ': xs) = (CToDef sch x, CanToConvert sch xs)

class
  ( CanRecConvert sch (TRecFlds sch tab a)
  , CanFromConvert sch (TRecFrom sch tab a)
  , CanToConvert sch (TRecTo sch tab a)
  , ToStar (RecFldInfos sch tab a)
  , ToStar tab )
  => CRecDef (sch::Type) (tab :: Symbol) (a::Type) where

  type TRecFlds sch tab a   :: [FldRecDef Symbol Type]
  -- ^ fields from table
  type TRecTo sch tab a   :: [RefRecDef Symbol Type]
  -- ^ referencies to master table
  type TRecFrom sch tab a :: [RefRecDef Symbol Type]
  -- ^ referencies to detail table

type RecFldInfos sch tab a = Map FrdInfoSym0 (TRecFlds sch tab a)
