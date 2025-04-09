{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Schema.Rec where

import Data.Kind
import Data.Singletons.TH
import Data.Text (Text)
import Database.Schema.Def
import Database.Types.EmptyField
import GHC.TypeLits qualified as TL
import GHC.TypeError
import PgSchema.Util
import Prelude.Singletons as SP
import Text.Show.Singletons
import Util.TH.LiftType


singletons [d|
  data FieldInfo' s = FieldInfo
    { fieldName   :: s
    , fieldDbName :: s
    , fieldKind   :: RecField' s }
    deriving Show

  data RecField' s
    = RFEmpty s
    | RFPlain (FldDef' s)
    | RFToHere (RecRef' s)
    | RFFromHere (RecRef' s)
    deriving Show

  data RecRef' s = RecRef
    { recRefTab :: NameNS' s
    , recRefs   :: [Ref' s] }
    deriving Show

  data QueryRecord' s = QueryRecord
    { qTableName  :: NameNS' s
    , qFields     :: [QueryField' s] }
    deriving Show

  data Ref' s = Ref
    { fromName :: s -- db field name
    , fromDef  :: FldDef' s
    , toName   :: s
    , toDef    :: FldDef' s }
    deriving (Eq, Show)

  data QueryField' s
    = QFieldPlain (FieldPlain' s)
    | QFieldTo    (FieldRef' (QueryRecord' s) s) -- (children)
    | QFieldFrom  (FieldRef' (QueryRecord' s) s) -- (parent)
    | QFieldEmpty s
    deriving Show

  data DmlRecord' s = DmlRecord
    { iTableName   :: NameNS' s
    , iFields :: [DmlField' s] }
    deriving Show

  data FieldRef' r s = FieldRef
    { frName :: s
    , frDbName :: s
    , frRec :: r
    , frRefs :: [Ref' s] }
    deriving Show

  data FieldPlain' s = FieldPlain
    { fpName :: s
    , fpDbName :: s
    , fpFldDef :: FldDef' s }
    deriving Show

  data DmlField' s
    = DmlFieldPlain (FieldPlain' s)
    | DmlFieldTo (FieldRef' (DmlRecord' s) s) -- (children)
    deriving Show
  |]


promote [d|
  getRecField :: Eq s =>
    TabDef' s -> [(NameNS' s, RelDef' s)] -> [(NameNS' s, RelDef' s)] ->
    (s -> FldDef' s) -> s -> RecField' s
  getRecField (TabDef flds _ _) froms tos f s = find1 flds
    where
      find1 []     = find2 froms
      find1 (x:xs) = if x == s then RFPlain (f s) else find1 xs
      find2 []         = find3 tos
      find2 ((a,b):xs) = if nnsName a == s then RFFromHere (toFrom b) else find2 xs
      find3 []         = RFEmpty s
      find3 ((a,b):xs) = if nnsName a == s then RFToHere (toTo b) else find3 xs
      toFrom rd = RecRef
        { recRefTab = rdTo rd
        , recRefs = map conv (rdCols rd) }
      toTo rd = RecRef
        { recRefTab = rdFrom rd
        , recRefs = map conv (rdCols rd) }
      conv (s1,s2) = Ref
        { fromName = s1
        , toName = s2
        , fromDef = f s1
        , toDef = f s2 }

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

  justIPlain :: DmlField' s -> Maybe (FieldPlain' s)
  justIPlain (DmlFieldPlain ifp) = Just ifp
  justIPlain _ = Nothing

  justITo :: DmlField' s -> Maybe (FieldRef' (DmlRecord' s) s)
  justITo (DmlFieldTo ift) = Just ift
  justITo _ = Nothing

  subDmlRecord :: Eq s => QueryRecord' s -> DmlRecord' s -> Bool
  subDmlRecord (QueryRecord tn flds) ir2 =
    tn == iTableName ir2 && all check flds
    where
      check = \case
        QFieldPlain{} -> True
        QFieldEmpty{} -> True
        QFieldFrom{} -> False
        QFieldTo (FieldRef _ dbname1 qr1' _) ->
          case foldr sameFieldTo Nothing (iFields ir2) of
            Nothing -> False
            Just ir2' -> subDmlRecord qr1' ir2'
          where
            sameFieldTo DmlFieldPlain{} Nothing = Nothing
            sameFieldTo (DmlFieldTo (FieldRef _ dbname2 ir2' _)) Nothing =
              if dbname1 == dbname2 then Just ir2' else Nothing
            sameFieldTo _ justR = justR

  allDmlPlainB :: DmlRecord' s -> Bool
  allDmlPlainB  (DmlRecord _tn flds) = all isPlain flds
    where
      isPlain (DmlFieldPlain _) = True
      isPlain _ = False
  |]

type FieldInfoK = FieldInfo' Symbol
type RecFieldK = RecField' Symbol
type RecRefK = RecRef' Symbol
type RefK = Ref' Symbol
type QueryRecordK = QueryRecord' Symbol
type QueryFieldK = QueryField' Symbol
type DmlRecordK = DmlRecord' Symbol
type DmlFieldK = DmlField' Symbol
type DmlFieldToK = FieldRef' Symbol
type FieldPlaink = FieldPlain' Symbol
type FieldInfo = FieldInfo' Text
type RecField = RecField' Text
type RecRef = RecRef' Text
type Ref = Ref' Text
type QueryRecord = QueryRecord' Text
type QueryField = QueryField' Text
type DmlRecord = DmlRecord' Text
type DmlField = DmlField' Text
type DmlFieldTo = FieldRef' Text
type FieldPlain = FieldPlain' Text

--------- LiftType for TH ----------
instance LiftType Ref where
  liftType Ref{..} = [t| 'Ref $(liftType fromName) $(liftType fromDef)
    $(liftType toName) $(liftType toDef) |]

instance LiftType RecRef where
  liftType RecRef{..} =
    [t| 'RecRef $(liftType recRefTab) $(liftType recRefs) |]

instance LiftType RecField where
  liftType = \case
    RFEmpty s -> [t| 'RFEmpty $(liftType s) |]
    RFPlain fd -> [t| 'RFPlain $(liftType fd) |]
    RFToHere rr -> [t| 'RFToHere $(liftType rr) |]
    RFFromHere rr -> [t| 'RFFromHere $(liftType rr) |]

instance LiftType FieldInfo where
  liftType FieldInfo{..} = [t| 'FieldInfo $(liftType fieldName)
    $(liftType fieldDbName) $(liftType fieldKind) |]

--------------------

-- | instances will be generated by TH
class CFieldType sch (r :: Type) (n :: Symbol) where
  type TFieldType sch r n :: Type

genDefunSymbols [''TFieldType]

-- | instances will be generated by TH
class ToStar (TRecordInfo sch tab r) => CRecordInfo sch (tab :: NameNSK) r where
  type TRecordInfo sch tab r :: [FieldInfoK]

recordInfo :: forall sch t r. CRecordInfo sch t r => [FieldInfo]
recordInfo = demote @(TRecordInfo sch t r)

instance CRecordInfo sch t EmptyField where
  type TRecordInfo sch t EmptyField = '[]

-------------- CQueryRecord ---------

class
  ( CQueryFields db sch tab (TRecordInfo sch tab r) (TFieldTypeSym2 sch r)
  , ToStar (TQueryRecord db sch tab r) )
  => CQueryRecord (db::Type) (sch::Type) (tab::NameNSK) (r::Type) where
  type TQueryRecord db sch tab r :: QueryRecordK
  type TQueryRecord db sch tab r = 'QueryRecord tab
    (TQueryFields db sch tab (TRecordInfo sch tab r) (TFieldTypeSym2 sch r))

getQueryRecord
  :: forall db sch tab r -> CQueryRecord db sch tab r => QueryRecord
getQueryRecord db sch tab r = demote @(TQueryRecord db sch tab r)

instance (CSchema sch, ToStar t) => CQueryRecord db sch t EmptyField where

class CTypDef sch tn => CanConvert db sch (tn::NameNSK) (nullable::Bool) t

----- CQueryFields ----

class CQueryFields db sch (t::NameNSK) (fis :: [FieldInfoK]) (fun :: Symbol ~> Type) where
  type TQueryFields db sch t fis fun :: [QueryFieldK]

instance CQueryFields db sch t '[] fun where
  type TQueryFields db sch t '[] fun = '[]

instance
  ( CQueryField (TFieldKind sch t (FieldDbName x)) db sch t '(x, Apply fun (FieldName x))
  , CQueryFields db sch t xs fun)
  => CQueryFields db sch t (x ': xs) fun where
  type TQueryFields db sch t (x ': xs) fun  =
    TQueryField (TFieldKind sch t (FieldDbName x)) db sch t '(x, Apply fun (FieldName x))
    ': TQueryFields db sch t xs fun

----- Single CQueryField ----

class CQueryField (ft::FldKindK) db sch (t::NameNSK) (fi::(FieldInfoK,Type))
  where
    type TQueryField ft db sch t fi :: QueryFieldK

instance CQueryField fk db sch t '( 'FieldInfo n dbname ('RFEmpty s), r) where
  type TQueryField fk db sch t '( 'FieldInfo n dbname ('RFEmpty s), r) = 'QFieldEmpty n

instance
  ( CanConvert db sch (FdType fd) (FdNullable fd) ftype )
  => CQueryField 'FldPlain db sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype)
  where
    type TQueryField 'FldPlain db sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype) =
      'QFieldPlain ('FieldPlain n dbname fd)

instance ( CQueryRecord db sch (RecRefTab rr) recFrom )
  => CQueryField ('FldTo rd) db sch t '( 'FieldInfo n dbname ('RFToHere rr), recFrom) where
  type TQueryField ('FldTo rd) db sch t '( 'FieldInfo n dbname ('RFToHere rr), recFrom) =
    'QFieldTo
      (FieldRef n dbname (TQueryRecord db sch (RecRefTab rr) recFrom)
      (RecRefs rr))

instance
  ( CQueryRecord db sch (RecRefTab rr) (UnMaybe recTo)
  , Assert
    (HasNullableRefTo rd (TFldDefSym2 sch t) == IsMaybe recTo)
    (TL.TypeError
      ((TL.Text "Reference from table " :<>: TL.ShowType (RdFrom rd)
        :<>: TL.Text " to table " :<>: TL.ShowType (RecRefTab rr) :<>: TL.Text " should "
        :<>: TL.Text (If (IsMaybe recTo) "not " "") :<>: TL.Text "be nullable")
      :$$: TL.Text "Probably you have drop or add 'Maybe' to field : "
        :<>: TL.ShowType n)) )
  => CQueryField ('FldFrom rd) db sch t '( 'FieldInfo n dbname ('RFFromHere rr), recTo) where
  type TQueryField ('FldFrom rd) db sch t '( 'FieldInfo n dbname ('RFFromHere rr), recTo) =
    'QFieldFrom
      (FieldRef n dbname (TQueryRecord db sch (RecRefTab rr) (UnMaybe recTo))
      (RecRefs rr))

------ For Dml -------

type family IsMaybe (x :: Type) :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe a = 'False

type family UnMaybe (x :: Type) :: Type where
  UnMaybe (Maybe a) = a
  UnMaybe a = a

type RestMand sch t r rFlds =
  RestMandatory sch t (Map FieldDbNameSym0 (TRecordInfo sch t r) ++ rFlds)

type family AllDmlPlain db sch tab r where
  AllDmlPlain db sch t r = Assert
    (AllDmlPlainB (TDmlRecord db sch t r))
    (TL.TypeError
      ((TL.Text "Not all fields in record are 'plain' fields " :<>: TL.ShowType t
        :<>: TL.Text " in type " :<>: TL.ShowType r)))

--------- DmlRecord --------

class ( ToStar (TDmlRecord db sch tab r)
  , CDmlFields db sch tab (TRecordInfo sch tab r) (TFieldTypeSym2 sch r))
  => CDmlRecord (db::Type) (sch::Type) (tab::NameNSK) (r::Type) where
  type TDmlRecord db sch tab r :: DmlRecordK
  type TDmlRecord db sch tab r = 'DmlRecord tab
    (TDmlFields db sch tab (TRecordInfo sch tab r) (TFieldTypeSym2 sch r))

getDmlRecord
  :: forall db sch tab r -> CDmlRecord db sch tab r => DmlRecord
getDmlRecord db sch tab r = demote @(TDmlRecord db sch tab r)

class CDmlFields db sch (t::NameNSK) (fis :: [FieldInfoK]) (fun :: Symbol ~> Type) where
  type TDmlFields db sch t fis fun :: [DmlFieldK]

instance CDmlFields db sch t '[] fun where
  type TDmlFields db sch t '[] fun = '[]

instance
  ( CDmlField (TFieldKind sch t (FieldDbName x)) db sch t '(x, Apply fun (FieldName x))
  , CDmlFields db sch t xs fun)
  => CDmlFields db sch t (x ': xs) fun where
  type TDmlFields db sch t (x ': xs) fun =
    TDmlField (TFieldKind sch t (FieldDbName x)) db sch t '(x, Apply fun (FieldName x))
    ': TDmlFields db sch t xs fun

class CDmlField (ft::FldKindK) db sch (t::NameNSK) (fi::(FieldInfoK,Type))
  where
    type TDmlField ft db sch t fi :: DmlFieldK

instance
  ( CFldDef sch t dbname, fdef ~ TFldDef sch t dbname
  , CanConvert db sch (FdType fdef) (FdNullable fdef) ftype )
  => CDmlField 'FldPlain db sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype)
  where
    type TDmlField 'FldPlain db sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype) =
      DmlFieldPlain ('FieldPlain n dbname fd)

class ( CDmlFields db sch (RdFrom rd) (TRecordInfo sch (RdFrom rd) r) (TFieldTypeSym2 sch r) )
  => CDmlRecordChild (db::Type) (sch::Type) (rd::RelDefK) (r::Type)
  where
    type TDmlRecordChild db sch rd r :: DmlRecordK
    type TDmlRecordChild db sch rd r = 'DmlRecord (RdFrom rd)
      (TDmlFields db sch (RdFrom rd) (TRecordInfo sch (RdFrom rd) r) (TFieldTypeSym2 sch r))

instance (CDmlRecordChild db sch rd recFrom)
  => CDmlField ('FldTo rd) db sch t '( 'FieldInfo n dbname ('RFToHere rr), recFrom)
  where
    type TDmlField ('FldTo rd) db sch t '( 'FieldInfo n dbname ('RFToHere rr), recFrom) =
      'DmlFieldTo
        ('FieldRef n dbname (TDmlRecordChild db sch rd recFrom)
        (MkRefs rd (TFldDefSym2 sch (RdFrom rd)) (TFldDefSym2 sch t)))

type SubDml db sch t r r' = Assert
  (SubDmlRecord (TQueryRecord db sch t r') (TDmlRecord db sch t r))
  (TL.TypeError
    (TL.Text "Result record doesn't correspond to input record"
    :$$: TL.Text "Input: " :<>: TL.ShowType r
    :$$: TL.Text "Result: " :<>: TL.ShowType r'))

type UpdateReturning db sch t r r' =
  (CDmlRecord db sch t r, CQueryRecord db sch t r', SubDml db sch t r r')

------------------------- PG.:. ----------

-- instance OrField (TRecordInfo sch t1 r1) (TRecordInfo sch t2 r2) n ~ 'True
--   => CFieldType sch (r1 PG.:. r2) n where
--   type TFieldType sch (r1 PG.:. r2) n = RiFieldType (TFieldTypeSym2 sch r1)
--     (TFieldTypeSym2 sch r2) (TRecordInfo sch t1 r1) (TRecordInfo sch t2 r2) n

-- instance (CRecordInfo sch r1, CRecordInfo sch r2
--   , ToStar (TRecordInfo sch (r1 PG.:. r2))) =>
--     CRecordInfo sch (r1 PG.:. r2) where
--   type TRecordInfo sch (r1 PG.:. r2) = TRecordInfo sch r1 ++ TRecordInfo sch r2

-- instance
--   ( CSchema sch
--   , CQueryFields db sch t (TRecordInfo sch (r1 :. r2))
--     (TFieldTypeSym2 sch (r1 :. r2))
--   , ToStar (TQueryRecord db sch t (r1 :. r2)))
--   => CQueryRecord db sch t (r1 :. r2)

-- instance
--   ( CSchema sch, CDmlFields db sch t (TRecordInfo sch (r1 :. r2))
--     (TFieldTypeSym2 sch (r1 :. r2))
--   , ToStar (TDmlRecord db sch t (r1 :. r2)))
--   => CDmlRecord db sch t (r1 :. r2)
