{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DerivingStrategies #-}
module Database.Schema.Rec where

import Data.Kind
import Data.Maybe
import Data.Maybe.Singletons
import Data.Singletons.TH
import Data.Text (Text)
import Database.PostgreSQL.Convert
import Database.Schema.Def
import Database.Types.EmptyField
import GHC.TypeLits qualified as TL
import GHC.TypeError
import PgSchema.Util
import Prelude.Singletons as SP
import Text.Show.Singletons
import Util.TH.LiftType


singletons [d|
  data FieldInfo' s p = FieldInfo -- p == (NameNS' s)
    { fieldName   :: s
    , fieldDbName :: s
    , fieldKind   :: RecField' s p }
    deriving Show

  data RecField' s p
    = RFEmpty s
    | RFPlain (FldDef' s)
    | RFToHere p [Ref' s]
    | RFFromHere p [Ref' s]
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
    (s -> FldDef' s) -> s -> RecField' s (NameNS' s)
  getRecField (TabDef flds _ _) froms tos f s = find1 flds
    where
      find1 []     = find2 froms
      find1 (x:xs) = if x == s then RFPlain (f s) else find1 xs
      find2 []         = find3 tos
      find2 ((a,b):xs) = if nnsName a == s then toFrom b else find2 xs
      find3 []         = RFEmpty s
      find3 ((a,b):xs) = if nnsName a == s then toTo b else find3 xs
      toFrom rd = RFFromHere (rdTo rd) (map conv (rdCols rd))
      toTo rd = RFToHere (rdFrom rd) (map conv (rdCols rd))
      conv (s1,s2) = Ref
        { fromName = s1
        , toName = s2
        , fromDef = f s1
        , toDef = f s2 }

  hasNullable :: [Ref' s] -> Bool
  hasNullable = any (fdNullable . fromDef)

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

  plainInfos :: [(FieldInfo' s p, t)] -> [(NameNS' s, Bool, t)]
  plainInfos = mapMaybe conv
    where
      conv (fi,t) = case fieldKind fi of
        RFPlain fd -> Just (fdType fd, fdNullable fd, t)
        _ -> Nothing
  |]

type FieldInfoK = FieldInfo' Symbol
type RecFieldK = RecField' Symbol
type RefK = Ref' Symbol
type QueryRecordK = QueryRecord' Symbol
type QueryFieldK = QueryField' Symbol
type DmlRecordK = DmlRecord' Symbol
type DmlFieldK = DmlField' Symbol
type DmlFieldToK = FieldRef' Symbol
type FieldPlaink = FieldPlain' Symbol
type FieldInfo = FieldInfo' Text
type RecField = RecField' Text
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

instance LiftType p => LiftType (RecField p) where
  liftType = \case
    RFEmpty s -> [t| 'RFEmpty $(liftType s) |]
    RFPlain fd -> [t| 'RFPlain $(liftType fd) |]
    RFToHere t rr -> [t| 'RFToHere $(liftType t) $(liftType rr) |]
    RFFromHere t rr -> [t| 'RFFromHere $(liftType t) $(liftType rr) |]

instance LiftType p => LiftType (FieldInfo p) where
  liftType FieldInfo{..} = [t| 'FieldInfo $(liftType fieldName)
    $(liftType fieldDbName) $(liftType fieldKind) |]

-------------------- CRecordInfo
newtype RecordInfo = RecordInfo { unRecordInfo :: [FieldInfo RecordInfo ]}
  deriving Show
  deriving newtype (Semigroup, Monoid)

class MkRecField sch (rf :: RecFieldK NameNSK) t where
  mkRecField :: RecField RecordInfo

instance ToStar x => MkRecField sch (RFEmpty x) EmptyField where
  mkRecField = RFEmpty (demote @x)
instance MkRecField sch (RFPlain x) EmptyField where
  mkRecField = RFEmpty mempty
instance MkRecField sch (RFToHere tab x) EmptyField where
  mkRecField = RFEmpty mempty
instance MkRecField sch (RFFromHere tab x) EmptyField where
  mkRecField = RFEmpty mempty
instance {-# OVERLAPPABLE #-}
  (ToStar x, CanConvert sch (FdType x) (FdNullable x) t) =>
    MkRecField sch (RFPlain x) t where
      mkRecField = RFPlain (demote @x)
instance {-# OVERLAPPABLE #-} (ToStar x, CRecordInfo sch tab t) =>
  MkRecField sch (RFToHere tab x) t where
    mkRecField = RFToHere (getRecordInfo @sch @tab @t) (demote @x)
instance {-# OVERLAPPABLE #-} (ToStar rr, CRecordInfo sch tab (UnMaybe t)
  , Assert (HasNullable rr == IsMaybe t)
    (TL.TypeError
      ( TL.Text "Condition for mandatory in DB not correspond to field type"
      :$$: TL.Text "Reference: "
      :$$: TL.ShowType rr
      :$$: TL.Text "Field: "
      :$$: TL.ShowType t))) =>
  MkRecField sch (RFFromHere tab rr) t where
  mkRecField = RFFromHere (getRecordInfo @sch @tab @(UnMaybe t)) (demote @rr)

-- | instances will be generated by TH
class
  CRecordInfo sch (tab :: NameNSK) r where
    type TRecordInfo sch tab r :: [(FieldInfoK NameNSK, Type)]
    getRecordInfo :: RecordInfo

instance CRecordInfo sch t EmptyField where
  type TRecordInfo sch t EmptyField = '[]
  getRecordInfo = RecordInfo []

-- type family AllConv sch (fis :: [(FieldInfoK NameNSK, Type)]) :: Constraint where
--   AllConv sch '[] = ()
--   AllConv sch ('( 'FieldInfo n dbn ('RFEmpty s), t) ': xs ) = AllConv sch xs
--   AllConv sch
--     ('( 'FieldInfo n dbn ('RFPlain ('FldDef ft fnull fdef)), t ) ': xs ) =
--       (CanConvert sch ft fnull t, AllConv sch xs)
--   AllConv sch ('( 'FieldInfo n dbn ('RFToHere tab rr), recFrom) ': xs) =
--     (CRecordInfo sch tab recFrom, AllConv sch xs)
--   AllConv sch ('( 'FieldInfo n dbn ('RFFromHere tab rr), recTo) ': xs) =
--     ( CRecordInfo sch tab (UnMaybe recTo)
--       , Assert (HasNullable rr == IsMaybe recTo)
--         (TL.TypeError
--           ((TL.Text "Reference for record field '" :<>: TL.ShowType n
--             :<>: TL.Text "' (db-constraint name: '" :<>: TL.ShowType dbn :<>: TL.Text "')"
--             :<>: TL.Text " to table " :<>: TL.ShowType tab :<>: TL.Text " should "
--             :<>: TL.Text (If (IsMaybe recTo) "not " "") :<>: TL.Text "be nullable")
--           :$$: TL.Text "Probably you have drop or add 'Maybe' to field : "
--             :<>: TL.ShowType n)) )

class CRecordInfo sch tab r => CQueryRecord' sch tab r where
  getQueryRecord' :: QueryRecord


-- type family AllPlainConv sch (fis :: [(FieldInfoK, Type)]) :: Constraint where
--   AllPlainConv sch '[] = ()
--   AllPlainConv sch
--     ('( 'FieldInfo n dbn ('RFPlain ('FldDef ft fnull fdef)), t ) ': xs ) =
--       (CanConvert sch ft fnull t, AllPlainConv sch xs)
--   AllPlainConv sch xs = ()

-- type family AllRefToHereConv sch (fis :: [(FieldInfoK, Type)]) :: Constraint where
--   AllRefToHereConv sch '[] = ()
--   AllRefToHereConv sch ('( 'FieldInfo n dbn ('RFToHere rr), recFrom) ': xs) =
--     (CRecordInfo sch (RecRefTab rr) recFrom, AllRefToHereConv sch xs)
--   AllRefToHereConv sch xs = ()

-------------- CQueryRecord ---------

class
  ( CQueryFields sch tab (TRecordInfo sch tab r)
  , ToStar (TQueryRecord sch tab r) )
  => CQueryRecord (sch::Type) (tab::NameNSK) (r::Type) where
  type TQueryRecord sch tab r :: QueryRecordK
  type TQueryRecord sch tab r =
    'QueryRecord tab (TQueryFields sch tab (TRecordInfo sch tab r))

getQueryRecord :: forall sch tab r -> CQueryRecord sch tab r => QueryRecord
getQueryRecord sch tab r = demote @(TQueryRecord sch tab r)

instance (CSchema sch, ToStar t) => CQueryRecord sch t EmptyField where

----- CQueryFields ----

class CQueryFields sch (t::NameNSK) (fis :: [(FieldInfoK NameNSK, Type)]) where
  type TQueryFields sch t fis :: [QueryFieldK]

instance CQueryFields sch t '[] where
  type TQueryFields sch t '[] = '[]

instance
  ( CQueryField sch t x , CQueryFields sch t xs)
  => CQueryFields sch t ( x ': xs) where
  type TQueryFields sch t ( x ': xs) =
    TQueryField sch t x ': TQueryFields sch t xs

----- Single CQueryField ----

class CQueryField sch (t::NameNSK) (fi::(FieldInfoK NameNSK,Type))
  where
    type TQueryField sch t fi :: QueryFieldK

instance CQueryField sch t '( 'FieldInfo n dbname ('RFEmpty s), r) where
  type TQueryField sch t '( 'FieldInfo n dbname ('RFEmpty s), r) = 'QFieldEmpty n

instance CQueryField sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype)
  where
    type TQueryField sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype) =
      'QFieldPlain ('FieldPlain n dbname fd)

instance ( CQueryRecord sch t2 recFrom )
  => CQueryField sch t '( 'FieldInfo n dbn ('RFToHere t2 rr), recFrom) where
  type TQueryField sch t '( 'FieldInfo n dbn ('RFToHere t2 rr), recFrom) =
    'QFieldTo (FieldRef n dbn (TQueryRecord sch t2 recFrom) rr)

instance
  ( CQueryRecord sch t2 (UnMaybe recTo) )
  -- , Assert (HasNullable rr == IsMaybe recTo)
  --   (TL.TypeError
  --     ((TL.Text "Reference from table " :<>: TL.ShowType t
  --       :<>: TL.Text " to table " :<>: TL.ShowType (RecRefTab rr) :<>: TL.Text " should "
  --       :<>: TL.Text (If (IsMaybe recTo) "not " "") :<>: TL.Text "be nullable")
  --     :$$: TL.Text "Probably you have drop or add 'Maybe' to field : "
  --       :<>: TL.ShowType n)) )
  => CQueryField sch t '( 'FieldInfo n dbn ('RFFromHere t2 rr), recTo) where
  type TQueryField sch t '( 'FieldInfo n dbn ('RFFromHere t2 rr), recTo) =
    'QFieldFrom (FieldRef n dbn (TQueryRecord sch t2 (UnMaybe recTo)) rr)

type family IsMaybe (x :: Type) :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe a = 'False

type family UnMaybe (x :: Type) :: Type where
  UnMaybe (Maybe a) = a
  UnMaybe a = a

promote [d|
  dbNames :: [(FieldInfo' s p, x)] -> [s]
  dbNames = map (fieldDbName . fst)
  |]

------ For Dml -------

type RestMand sch t r rFlds =
  RestMandatory sch t (DbNames (TRecordInfo sch t r) ++ rFlds)

type family AllDmlPlain sch tab r where
  AllDmlPlain sch t r = Assert
    (AllDmlPlainB (TDmlRecord sch t r))
    (TL.TypeError
      ((TL.Text "Not all fields in record are 'plain' fields " :<>: TL.ShowType t
        :<>: TL.Text " in type " :<>: TL.ShowType r)))

--------- DmlRecord --------

class ( ToStar (TDmlRecord sch tab r)
  , CDmlFields sch tab (TRecordInfo sch tab r))
  => CDmlRecord (sch::Type) (tab::NameNSK) (r::Type) where
  type TDmlRecord sch tab r :: DmlRecordK
  type TDmlRecord sch tab r =
    'DmlRecord tab (TDmlFields sch tab (TRecordInfo sch tab r))

getDmlRecord
  :: forall sch tab r -> CDmlRecord sch tab r => DmlRecord
getDmlRecord sch tab r = demote @(TDmlRecord sch tab r)

class CDmlFields sch (t::NameNSK) (fis :: [(FieldInfoK NameNSK, Type)]) where
  type TDmlFields sch t fis :: [DmlFieldK]

instance CDmlFields sch t '[] where
  type TDmlFields sch t '[]  = '[]

instance (CDmlField sch t x, CDmlFields sch t xs)
  => CDmlFields sch t (x ': xs) where
  type TDmlFields sch t (x ': xs) =
    TDmlField sch t x ': TDmlFields sch t xs

class CDmlField sch (t::NameNSK) (fi::(FieldInfoK NameNSK,Type))
  where
    type TDmlField sch t fi :: DmlFieldK

instance CDmlField sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype)
  where
    type TDmlField sch t '( 'FieldInfo n dbname ('RFPlain fd), ftype) =
      DmlFieldPlain ('FieldPlain n dbname fd)

instance ( CDmlRecord sch t2 recFrom )
  => CDmlField sch t '( 'FieldInfo n dbname ('RFToHere t2 rr), recFrom) where
  type TDmlField sch t '( 'FieldInfo n dbname ('RFToHere t2 rr), recFrom) =
    'DmlFieldTo (FieldRef n dbname (TDmlRecord sch t2 recFrom) rr)

type SubDml sch t r r' = Assert
  (SubDmlRecord (TQueryRecord sch t r') (TDmlRecord sch t r))
  (TL.TypeError
    (TL.Text "Result record doesn't correspond to input record"
    :$$: TL.Text "Input: " :<>: TL.ShowType r
    :$$: TL.Text "Result: " :<>: TL.ShowType r'))

type UpdateReturning sch t r r' =
  (CDmlRecord sch t r, CQueryRecord sch t r', SubDml sch t r r')

------------------------- PG.:. ----------

-- Now CRecordInfo depends on param "table name", so it is unclear how to
-- make product of records


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
--   , ToStar (TQueryRecord sch t (r1 :. r2)))
--   => CQueryRecord sch t (r1 :. r2)

-- instance
--   ( CSchema sch, CDmlFields db sch t (TRecordInfo sch (r1 :. r2))
--     (TFieldTypeSym2 sch (r1 :. r2))
--   , ToStar (TDmlRecord sch t (r1 :. r2)))
--   => CDmlRecord sch t (r1 :. r2)
