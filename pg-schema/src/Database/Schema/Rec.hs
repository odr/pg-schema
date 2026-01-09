{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DerivingStrategies #-}
module Database.Schema.Rec where

import Data.Kind
import Data.Maybe
import Data.Maybe.Singletons
import Data.Singletons.TH
import Data.Text as T (Text, pack)
import Database.PostgreSQL.Convert
import Database.Schema.Def
import Database.Types.Aggr
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
    | RFAggr (FldDef' s) s Bool
    -- True in RFAggr means: "Can be used in any selects",
    -- False means: "Can be used only in selects with GROUP BY"
    | RFToHere p [Ref' s]
    | RFFromHere p [Ref' s]
    deriving Show

  data Ref' s = Ref
    { fromName :: s -- db field name
    , fromDef  :: FldDef' s
    , toName   :: s
    , toDef    :: FldDef' s }
    deriving (Eq, Show)
  |]

promote [d|
  getRecField :: Eq s =>
    NameNS' s -> TabDef' s -> [(NameNS' s, RelDef' s)] -> [(NameNS' s, RelDef' s)] ->
    (NameNS' s -> s -> FldDef' s) -> s -> RecField' s (NameNS' s)
  getRecField tabName (TabDef flds _ _) froms tos f s = find1 flds
    where
      find1 []     = find2 froms
      find1 (x:xs) = if x == s then RFPlain (f tabName s) else find1 xs
      find2 []         = find3 tos
      find2 ((a,b):xs) = if nnsName a == s then toFrom b else find2 xs
      find3 []         = RFEmpty s
      find3 ((a,b):xs) = if nnsName a == s then toTo b else find3 xs
      toFrom rd = RFFromHere (rdTo rd) (map conv (rdCols rd))
        where
          conv (s1,s2) = Ref
            { fromName = s1
            , toName = s2
            , fromDef = f tabName s1
            , toDef = f (rdTo rd) s2 }
      toTo rd = RFToHere (rdFrom rd) (map conv (rdCols rd))
        where
          conv (s1,s2) = Ref
            { fromName = s1
            , toName = s2
            , fromDef = f (rdFrom rd) s1
            , toDef = f tabName s2 }

  hasNullable :: [Ref' s] -> Bool
  hasNullable = any (fdNullable . fromDef)

  -- subDmlRecord :: Eq s => QueryRecord' s -> DmlRecord' s -> Bool
  -- subDmlRecord (QueryRecord tn flds) ir2 =
  --   tn == iTableName ir2 && all check flds
  --   where
  --     check = \case
  --       QFieldPlain{} -> True
  --       QFieldEmpty{} -> True
  --       QFieldFrom{} -> False
  --       QFieldTo (FieldRef _ dbname1 qr1' _) ->
  --         case foldr sameFieldTo Nothing (iFields ir2) of
  --           Nothing -> False
  --           Just ir2' -> subDmlRecord qr1' ir2'
  --         where
  --           sameFieldTo DmlFieldPlain{} Nothing = Nothing
  --           sameFieldTo (DmlFieldTo (FieldRef _ dbname2 ir2' _)) Nothing =
  --             if dbname1 == dbname2 then Just ir2' else Nothing
  --           sameFieldTo _ justR = justR

  allPlainB :: [(FieldInfo' s p, t)] -> Bool
  allPlainB flds = all (isPlain . fieldKind . fst) flds
    where
      isPlain (RFPlain _) = True
      isPlain _ = False

  plainInfos :: [(FieldInfo' s p, t)] -> [(NameNS' s, Bool, t)]
  plainInfos = mapMaybe conv
    where
      conv (fi,t) = case fieldKind fi of
        RFPlain fd -> Just (fdType fd, fdNullable fd, t)
        _ -> Nothing
  |]

-- subRec :: [FieldInfo' (NameNS' s) s] -> [FieldInfo' (NameNS' s) s] -> Bool
-- subRec fis1 fis2 = all check1 fis1
--   where
--     check1 fi1 = case fi1.fieldKind of
--       RFEmpty{} -> True
--       RFPlain{} -> True
--       RFFromHere{} -> undefined
--       RFToHere tn1 refs1 -> maybe False (const True) $ L.find check2 fis2
--         where
--           check2 fi2 = fi2.fieldDbName == fi1.fieldDbName
--             && case fi2.fieldKind of
--               RFToHere tn2 refs2


type FieldInfoK = FieldInfo' Symbol
type RecFieldK = RecField' Symbol
type RefK = Ref' Symbol
type FieldInfo = FieldInfo' Text
type RecField = RecField' Text
type Ref = Ref' Text

data RecordInfo = RecordInfo
  { tabName :: NameNS
  , fields :: [FieldInfo RecordInfo] }
  deriving Show

--------- LiftType for TH ----------
instance LiftType Ref where
  liftType Ref{..} = [t| 'Ref $(liftType fromName) $(liftType fromDef)
    $(liftType toName) $(liftType toDef) |]

instance LiftType p => LiftType (RecField p) where
  liftType = \case
    RFEmpty s -> [t| 'RFEmpty $(liftType s) |]
    RFPlain fd -> [t| 'RFPlain $(liftType fd) |]
    RFAggr fd fname b -> [t| 'RFAggr $(liftType fd) $(liftType fname) $(liftType b)|]
    RFToHere t rr -> [t| 'RFToHere $(liftType t) $(liftType rr) |]
    RFFromHere t rr -> [t| 'RFFromHere $(liftType t) $(liftType rr) |]

instance LiftType p => LiftType (FieldInfo p) where
  liftType FieldInfo{..} = [t| 'FieldInfo $(liftType fieldName)
    $(liftType fieldDbName) $(liftType fieldKind) |]

-------------------- CRecordInfo

class MkRecField sch (rf :: RecFieldK NameNSK) t where
  mkRecField :: RecField RecordInfo

instance ToStar x => MkRecField sch (RFEmpty x) EmptyField where
  mkRecField = RFEmpty (demote @x)
instance MkRecField sch (RFPlain x) EmptyField where
  mkRecField = RFEmpty $ T.pack "empty"
instance MkRecField sch (RFToHere tab x) EmptyField where
  mkRecField = RFEmpty $ T.pack "empty"
instance MkRecField sch (RFFromHere tab x) EmptyField where
  mkRecField = RFEmpty $ T.pack "empty"
instance {-# OVERLAPPABLE #-}
  (ToStar x, CanConvert sch (FdType x) (FdNullable x) t) =>
    MkRecField sch (RFPlain x) t where
      mkRecField = RFPlain (demote @x)
instance {-# OVERLAPPING #-}
  (ToStar x, CanConvert sch (FdType x) (FdNullable x) t) =>
    MkRecField sch (RFAggr x "count" True) (Aggr' "count" t) where
      mkRecField = RFAggr (demote @x) (demote @"count") False
instance {-# OVERLAPPABLE #-}
  (ToStar x, ToStar fn, CanConvert sch (FdType x) (FdNullable x) t) =>
    MkRecField sch (RFAggr x fn False) (Aggr' fn t) where
      mkRecField = RFAggr (demote @x) (demote @fn) False
instance {-# OVERLAPPABLE #-}
  (ToStar x, ToStar fn, CanConvert sch (FdType x) (FdNullable x) t) =>
    MkRecField sch (RFAggr x fn True) (Aggr fn t) where
      mkRecField = RFAggr (demote @x) (demote @fn) True
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

instance ToStar t => CRecordInfo sch t EmptyField where
  type TRecordInfo sch t EmptyField = '[]
  getRecordInfo = RecordInfo (demote @t) []

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

type RestPKFlds sch t r rFlds =
  RestPK sch t (DbNames (TRecordInfo sch t r) ++ rFlds)

type family AllPlain sch tab r where
  AllPlain sch t r = Assert
    (AllPlainB (TRecordInfo sch t r))
    (TL.TypeError
      (TL.Text "Not all fields in record are 'plain' fields "
        :$$: TL.Text "Table: " :<>: TL.ShowType t
        :$$: TL.Text "Type: " :<>: TL.ShowType r
        :$$: TL.Text "Record Info: " :<>: TL.ShowType (TRecordInfo sch t r)))

type UpdateReturning sch t r r' = (CRecordInfo sch t r, CRecordInfo sch t r')
