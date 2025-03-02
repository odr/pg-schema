{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Database.Schema.Rec where

import Data.Kind
import Data.Singletons.TH
import Data.Text (Text)
import Database.PostgreSQL.Simple.Types as PG
import Database.Schema.Def
import PgSchema.Util
import Prelude.Singletons as SP
import Text.Show.Singletons


singletons [d|
  data FieldInfo' s = FieldInfo
    { fieldName :: s
    , fieldDbName :: s }
    deriving Show

  data QueryRecord' s = QueryRecord
    { qTableName   :: NameNS' s
    , qFields :: [QueryField' s] }
    deriving Show

  data Ref' s = Ref
    { fromName :: s -- db field name
    , fromDef  :: FldDef' s
    , toName   :: s
    , toDef    :: FldDef' s }
    deriving Show

  data QueryField' s
    = QFieldPlain s s (FldDef' s) -- name dbname flddef
    | QFieldTo    s s (QueryRecord' s) [Ref' s] -- (children)
    | QFieldFrom  s s (QueryRecord' s) [Ref' s] -- (parent)
    deriving Show

  data InsertRecordChild s = InsertRecord
    { iTableName   :: NameNS' s
    , iFields :: [InsertField' s] }
    deriving Show

  data InsertField' s
    = IFieldPlain s s (FldDef' s) -- name dbname flddef
    | IFieldTo    s s (InsertRecordChild s) [Ref' s] -- (children)
    deriving Show
  |]

promote [d|
  fiWithType :: (s -> t) -> [FieldInfo' s] -> [(FieldInfo' s, t)]
  fiWithType f = map (\fi -> (fi, f $ fieldName fi))

  riFieldType
    :: Eq s
    => (s -> t) -> (s -> t) -> [FieldInfo' s] -> [FieldInfo' s] -> s -> t
  riFieldType f1 f2 rs1 rs2 n = find1 rs1
    where
      find1 []     = find2 rs2
      find1 (x:xs) = if fieldName x == n then f1 n else find1 xs
      find2 []     = error "riFieldType: No field found"
      find2 (x:xs) = if fieldName x == n then f2 n else find2 xs

  orField :: Eq s => [FieldInfo' s] -> [FieldInfo' s] -> s -> Bool
  orField rs1 rs2 n = find1 rs1
    where
      find1 []     = find2 rs2
      find1 (x:xs) = fieldName x == n || find1 xs
      find2 []     = False
      find2 (x:xs) = fieldName x == n || find2 xs

  mkRefs :: RelDef' s -> (s -> FldDef' s) -> (s -> FldDef' s) -> [Ref' s]
  mkRefs rd f1 f2 = mkRefs' cols (map f1 cols1) (map f2 cols2)
    where
      mkRefs' = zipWith3 (\(fromName,toName) fromDef toDef -> Ref {..})
      cols = rdCols rd
      (cols1, cols2) = unzip cols

  hasNullableRefTo :: RelDef' s -> (s -> FldDef' s) -> Bool
  hasNullableRefTo rd f = any (fdNullable . f) $ fst $ unzip $ rdCols rd

  |]

type FieldInfoK = FieldInfo' Symbol
type RefK = Ref' Symbol
type QueryRecordK = QueryRecord' Symbol
type QueryFieldK = QueryField' Symbol
type InsertRecordK = InsertRecordChild Symbol
type InsertFieldK = InsertField' Symbol
type FieldInfo = FieldInfo' Text
type Ref = Ref' Text
type QueryRecord = QueryRecord' Text
type QueryField = QueryField' Text
type InsertRecord = InsertRecordChild Text
type InsertField = InsertField' Text

-- | instances will be generated by TH
class CFieldType (r :: Type) (n :: Symbol) where
  type TFieldType r n :: Type

genDefunSymbols [''TFieldType]

instance OrField (TRecordInfo r1) (TRecordInfo r2) n ~ 'True
  => CFieldType (r1 PG.:. r2) n where
  type TFieldType (r1 PG.:. r2) n = RiFieldType
    (TFieldTypeSym1 r1) (TFieldTypeSym1 r2) (TRecordInfo r1) (TRecordInfo r2) n

-- | instances will be generated by TH
class ToStar (TRecordInfo r) => CRecordInfo r where
  type TRecordInfo r :: [FieldInfoK]

instance (CRecordInfo r1, CRecordInfo r2, ToStar (TRecordInfo (r1 PG.:. r2)))
  => CRecordInfo (r1 PG.:. r2) where
  type TRecordInfo (r1 PG.:. r2) = TRecordInfo r1 ++ TRecordInfo r2

instance
  ( CQueryRecord db sch t r1, CQueryRecord db sch t r2
  , CQueryFields db sch t (FiTypeInfo (r1 :. r2)) )
  => CQueryRecord db sch t (r1 :. r2)

recordInfo :: forall r. CRecordInfo r => [FieldInfo]
recordInfo = demote @(TRecordInfo r)

type FiTypeInfo r = FiWithType (TFieldTypeSym1 r) (TRecordInfo r)

-- record over table
class
  ( CQueryFields db sch tab (FiTypeInfo r)
  , ToStar (TQueryRecord db sch tab r) )
  => CQueryRecord (db::Type) (sch::Type) (tab::NameNSK) (r::Type) where
  type TQueryRecord db sch tab r :: QueryRecordK
  type TQueryRecord db sch tab r = 'QueryRecord tab
    (TQueryFields db sch tab (FiTypeInfo r))

getQueryRecord
  :: forall db sch tab r. CQueryRecord db sch tab r => QueryRecord
getQueryRecord  = demote @(TQueryRecord db sch tab r)

class CTypDef sch tn => CanConvert db sch (tn::NameNSK) (nullable::Bool) t

-- classify fields of record over table: Plain, RefTo, RefFrom
class (CSchema sch, CTabDef sch t, ToStar (TQueryFields db sch t fis))
  => CQueryFields db sch (t::NameNSK) (fis :: [(FieldInfoK,Type)]) where
  type TQueryFields db sch t fis :: [QueryFieldK]

getQueryFields :: forall db sch t fis. CQueryFields db sch t fis => [QueryField]
getQueryFields = demote @(TQueryFields db sch t fis)

instance (CSchema sch, CTabDef sch t) => CQueryFields db sch t '[] where
  type TQueryFields db sch t '[] = '[]

instance
  ( CQueryField (TFieldKind sch t (FieldDbName (Fst x))) db sch t x
  , CQueryFields db sch t xs, ToStar (TQueryFields db sch t (x ': xs)))
  => CQueryFields db sch t (x ': xs) where
  type TQueryFields db sch t (x ': xs) =
    TQueryField (TFieldKind sch t (FieldDbName (Fst x))) db sch t x
    ': TQueryFields db sch t xs

class CQueryField (ft::FldKindK) db sch (t::NameNSK) (fi::(FieldInfoK,Type))
  where
    type TQueryField ft db sch t fi :: QueryFieldK

instance
  ( CFldDef sch t dbname, fdef ~ TFldDef sch t dbname, ToStar n
  , CanConvert db sch (FdType fdef) (FdNullable fdef) ftype )
  => CQueryField 'FldPlain db sch t '( 'FieldInfo n dbname, ftype)
  where
    type TQueryField 'FldPlain db sch t '( 'FieldInfo n dbname, ftype) =
      'QFieldPlain n dbname (TFldDef sch t dbname)

instance ( CQueryRecord db sch (RdFrom rd) recFrom )
  => CQueryField ('FldTo rd) db sch t '( 'FieldInfo n dbname, recFrom) where
    type TQueryField ('FldTo rd) db sch t '( 'FieldInfo n dbname, recFrom) =
      'QFieldTo n dbname (TQueryRecord db sch (RdFrom rd) recFrom)
        (MkRefs rd (TFldDefSym2 sch (RdFrom rd)) (TFldDefSym2 sch t))

instance
  ( CQueryRecord db sch (RdTo rd) (Snd (UnMaybe recTo))
  , HasNullableRefTo rd (TFldDefSym2 sch t) ~ Fst (UnMaybe recTo) )
  => CQueryField ('FldFrom rd) db sch t '( 'FieldInfo n dbname, recTo)
  where
    type TQueryField ('FldFrom rd) db sch t '( 'FieldInfo n dbname, recTo) =
      'QFieldFrom n dbname (TQueryRecord db sch (RdTo rd) (Snd (UnMaybe recTo)))
        (MkRefs rd (TFldDefSym2 sch t) (TFldDefSym2 sch (RdTo rd)))
--

type family UnMaybe (x :: Type) :: (Bool, Type) where
  UnMaybe (Maybe a) = '( 'True, a)
  UnMaybe a = '( 'False, a)

type AllMandatory sch t r rFlds =
  IsAllMandatory sch t (Map FieldDbNameSym0 (TRecordInfo r) ++ rFlds) ~ 'True

class ( ToStar (TInsertRecord db sch tab r), AllMandatory sch tab r '[] )
  => CInsertRecord (db::Type) (sch::Type) (tab::NameNSK) (r::Type) where
  type TInsertRecord db sch tab r :: InsertRecordK
  type TInsertRecord db sch tab r = 'InsertRecord tab
    (TInsertFields db sch tab (FiTypeInfo r))

getInsertRecord
  :: forall db sch tab r. CInsertRecord db sch tab r => InsertRecord
getInsertRecord  = demote @(TInsertRecord db sch tab r)

class
  ( CInsertFields db sch (RdFrom rd) (FiTypeInfo r)
  , ToStar (TInsertRecordChild db sch rd r )
  , AllMandatory sch (RdFrom rd) r ((Map FstSym0 (RdCols rd))) )
  => CInsertRecordChild (db::Type) (sch::Type) (rd::RelDefK) (r::Type) where
  type TInsertRecordChild db sch rd r :: InsertRecordK
  type TInsertRecordChild db sch rd r = 'InsertRecord (RdFrom rd)
    (TInsertFields db sch (RdFrom rd) (FiTypeInfo r))

class (CSchema sch, CTabDef sch t, ToStar (TInsertFields db sch t fis))
  => CInsertFields db sch (t::NameNSK) (fis :: [(FieldInfoK,Type)]) where
  type TInsertFields db sch t fis :: [InsertFieldK]

instance (CSchema sch, CTabDef sch t) => CInsertFields db sch t '[] where
  type TInsertFields db sch t '[] = '[]

instance
  ( CInsertField (TFieldKind sch t (FieldDbName (Fst x))) db sch t x
  , CInsertFields db sch t xs, ToStar (TInsertFields db sch t (x ': xs)))
  => CInsertFields db sch t (x ': xs) where
  type TInsertFields db sch t (x ': xs) =
    TInsertField (TFieldKind sch t (FieldDbName (Fst x))) db sch t x
    ': TInsertFields db sch t xs

class CInsertField (ft::FldKindK) db sch (t::NameNSK) (fi::(FieldInfoK,Type))
  where
    type TInsertField ft db sch t fi :: InsertFieldK

instance
  ( CFldDef sch t dbname, fdef ~ TFldDef sch t dbname, ToStar n
  , CanConvert db sch (FdType fdef) (FdNullable fdef) ftype )
  => CInsertField 'FldPlain db sch t '( 'FieldInfo n dbname, ftype)
  where
    type TInsertField 'FldPlain db sch t '( 'FieldInfo n dbname, ftype) =
      'IFieldPlain n dbname (TFldDef sch t dbname)

instance (CInsertRecordChild db sch rd recFrom)
  => CInsertField ('FldTo rd) db sch t '( 'FieldInfo n dbname, recFrom)
  where
    type TInsertField ('FldTo rd) db sch t '( 'FieldInfo n dbname, recFrom) =
      'IFieldTo n dbname (TInsertRecord db sch (RdFrom rd) recFrom)
        (MkRefs rd (TFldDefSym2 sch (RdFrom rd)) (TFldDefSym2 sch t))
