{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS
import Control.Monad
import Data.Bifunctor
import Data.Foldable as F
import Data.Functor
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Singletons
import Data.String
import Data.Text as T
import Data.Tuple
import Database.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple hiding(In(..))
import Database.Schema.Def
import Database.Schema.Rec
import Database.Schema.ShowType
import GHC.TypeLits
import PgSchema.Util
import Prelude as P


selectSch :: forall sch tab r.
  (FromRow r, CRecordInfo sch tab r) =>
  Connection -> QueryParam sch tab -> IO ([r], (Text,[SomeToField]))
selectSch conn (selectText @sch @tab @r -> (sql,fs)) =
  trace' (T.unpack sql <> "\n\n" <> P.show fs <> "\n\n") $ (,(sql,fs)) <$> query conn (fromString $ unpack sql) fs

selectQuery :: forall sch tab r. (CRecordInfo sch tab r) =>
  QueryParam sch tab -> (Query,[SomeToField])
selectQuery = first (fromString . unpack) . selectText @sch @tab @r

selectText :: forall sch t r. (CRecordInfo sch t r) =>
  QueryParam sch t -> (Text,[SomeToField])
selectText qp = evalRWS (selectM (getRecordInfo @sch @t @r)) (qr0 qp) qs0

qr0 :: QueryParam sch t -> QueryRead sch t
qr0 qrParam = QueryRead
  { qrCurrTabNum = 0 , qrIsRoot = True , qrPath = [] , qrParam }

qs0 :: QueryState
qs0 = QueryState { qsLastTabNum = 0, qsJoins = [], qsHasWhere = False
  , qsOrd = "", qsLimOff = "" }

two :: (a,b,c) -> (a,b)
two (a,b,_) = (a,b)

third :: (a,b,c) -> c
third (_,_,c) = c

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = mapMaybe (\(a,b) -> if "$EmptyField" `T.isSuffixOf` b then Nothing
      else Just $ "'" <> b <> "'," <> a) fs

selectM :: MonadQuery sch t m => RecordInfo -> m Text
selectM ri = do
  QueryRead {..} <- ask
  (fmap two -> flds) <- traverse fieldM ri.fields
  joins <- gets $ L.reverse . qsJoins
  let
    (condText, condPars) =
      condByPath qrCurrTabNum (L.reverse qrPath) qrParam.qpConds
    (ordText, ordPars) =
      ordByPath qrCurrTabNum (L.reverse qrPath) qrParam.qpOrds
    (distTexts, distPars) =
      distByPath qrCurrTabNum (L.reverse qrPath) qrParam.qpDistinct
    sel
      | qrIsRoot =
        T.intercalate "," $ L.map (\(a,b) -> a <> " \"" <> b <> "\"") flds
      | otherwise = jsonPairing flds
    whereText
      | condText == mempty = ""
      | otherwise = " where " <> condText
    orderText
      | T.null ordText && T.null distTexts.orderBy = ""
      | otherwise = " order by " <> T.intercalate ","
        (L.filter (not . T.null) [distTexts.orderBy, ordText])
    distinctText
      | distTexts.distinct = " distinct "
      | T.null distTexts.distinctOn = ""
      | otherwise = " distinct on (" <> distTexts.distinctOn <> ") "
    qsLimOff = loByPath (L.reverse qrPath) $ qpLOs qrParam
    groupByText
      | P.null aggrs || P.null others = ""
      | otherwise = " group by " <> T.intercalate ","
        (L.nub $ others >>= \fi -> case fi.fieldKind of
          RFPlain{} -> [fi.fieldDbName]
          RFToHere _ refs -> refs <&> \ref -> ref.toName
          RFFromHere _ refs -> refs <&> \ref -> ref.fromName
          _ -> []  )
      where
        (aggrs, others) = L.partition
          (\fld -> case fld.fieldKind of { RFAggr{} -> True; _ -> False })
          ri.fields
  modify (\qs -> qs { qsHasWhere = whereText /= mempty })
  unless qrIsRoot $ modify (\qs -> qs { qsOrd = orderText, qsLimOff })
  tell $ distPars <> condPars <> distPars <> ordPars
  pure $ "select "
    <> distinctText
    <> sel
    <> " from " <> qualName ri.tabName <> " t" <> show' qrCurrTabNum
    <> " " <> T.unwords joins
    <> whereText
    <> groupByText
    <> (if qrIsRoot then orderText else "")
    <> (if qrIsRoot then qsLimOff else "")

-- | return text for field, alias and expression to check is empty
-- (not obvious for FieldTo)
fieldM :: MonadQuery sch tab m => FieldInfo RecordInfo -> m (Text, Text, Text)
fieldM fi = case fi.fieldKind of
  RFEmpty s -> pure ("null", s, "true")
  RFPlain {} -> do
    n <- asks qrCurrTabNum
    let val = "t" <> show' n <> "." <> fi.fieldDbName
    pure (val, fi.fieldDbName, val <> " is null")
  RFAggr _ fname -> do
    n <- asks qrCurrTabNum
    let val = fname <> "(" <> "t" <> show' n <> "." <> fi.fieldDbName <> ")"
    pure case fname of
      "count" -> ("count(*)", fi.fieldName, " false")
      _ -> (val, fi.fieldName, val <> " is null")
  RFFromHere ri refs -> do
    QueryRead {..} <- ask
    modify \QueryState{qsLastTabNum, qsJoins} -> QueryState
      { qsLastTabNum = qsLastTabNum+1
      , qsJoins = joinText qrCurrTabNum (qsLastTabNum+1) : qsJoins
      , qsHasWhere = False
      , qsOrd = ""
      , qsLimOff = "" }
    n2 <- gets qsLastTabNum
    flds <- local
      (\qr -> qr{ qrCurrTabNum = n2, qrIsRoot = False, qrPath = fi.fieldDbName : qrPath })
      (traverse fieldM ri.fields)
    let val = fldt flds
    pure (val, fi.fieldDbName, val <> " is null")
    where
      nullable = L.any (fdNullable . fromDef) refs
      joinText n1 n2 =
        outer <> "join " <> qualName ri.tabName <> " t" <> show' n2
            <> " on " <> refCond n1 n2 refs
        where
          outer
            | nullable = "left outer "
            | otherwise = ""
      fldt flds
        | nullable = "case when " <> isNull <>
            " then null else " <> jsonPairing (two <$> flds) <> " end"
        | otherwise = jsonPairing $ two <$> flds
        where
          isNull = T.intercalate " and " $ third <$> flds
  RFToHere ri refs -> do
    QueryRead{..} <- ask
    QueryState {qsLastTabNum, qsJoins} <- get
    modify (const $ QueryState (qsLastTabNum+1) [] False "" "")
    selText <- local
      (\qr -> qr
        { qrCurrTabNum = qsLastTabNum+1, qrIsRoot = False
        , qrPath = fi.fieldDbName : qrPath })
      (selectM ri)
    modify (\qs -> qs { qsJoins = qsJoins })
    QueryState{qsHasWhere, qsOrd, qsLimOff} <- get
    let
      val = "array(" <> selText <> " " <> (if qsHasWhere then "and" else "where")
        <> " " <> refCond (qsLastTabNum+1) qrCurrTabNum refs
        <> qsOrd <> qsLimOff <> ")"
    pure ("array_to_json(" <> val <> ")", fi.fieldDbName, val <> " = '{}'")

refCond :: Int -> Int -> [Ref] -> Text
refCond nFrom nTo = T.intercalate " and " . fmap compFlds
  where
    compFlds Ref {fromName, toName} =
      fldt nFrom fromName <> "=" <> fldt nTo toName
      where
        fldt n = (("t" <> show' n <> ".") <>)

withLOWithPath
  :: forall sch t r. (LO -> r) -> [Text] -> LimOffWithPath sch t -> Maybe r
withLOWithPath f p (LimOffWithPath @p lo) =
  guard (p == demote @p) >> pure (f lo)

withLOsWithPath
  :: forall sch t r. (LO -> r) -> [Text] -> [LimOffWithPath sch t] -> Maybe r
withLOsWithPath f p = join . L.find isJust . L.map (withLOWithPath f p)

lowp :: forall sch t. forall (path::[Symbol]) ->
  (ToStar path, TabPath sch t path) => LO -> LimOffWithPath sch t
lowp p = LimOffWithPath @p

rootLO :: forall sch t. LO -> LimOffWithPath sch t
rootLO = lowp []

convLO :: LO -> Text
convLO (LO ml mo) =
  maybe "" ((" limit " <>) . show') ml
   <> maybe "" ((" offset " <>) . show') mo

loByPath :: forall sch t. [Text] -> [LimOffWithPath sch t] -> Text
loByPath p = fromMaybe mempty . withLOsWithPath convLO p

runCond :: Int -> CondMonad a -> (a,[SomeToField])
runCond n x = evalRWS x ("q", pure n) 0

tabPref :: CondMonad Text
tabPref = asks \case
  (_, n :| []) -> "t" <> show' n
  (p, n :| (np : _)) -> "t" <> show' np <> p <> show' n

qual :: forall (fld :: Symbol). ToStar fld => CondMonad Text
qual = tabPref <&> (<> "." <> (demote @fld))

--
convCond :: forall sch t . Cond sch t -> CondMonad Text
convCond = \case
  EmptyCond -> pure mempty
  Cmp @n cmp v -> do
    tell [SomeToField v]
    qual @n <&> (<> " " <> showCmp cmp <> " ?")
  In @n (NE.toList -> vs) -> do
    tell (SomeToField <$> vs)
    qual @n <&> (<> " in (" <> T.intercalate "," ("?" <$ vs) <> ")")
  Null @n -> qual @n <&> (<> " is null")
  Not c -> getNot <$> convCond c
  BoolOp bo c1 c2 -> getBoolOp bo <$> convCond c1 <*> convCond c2
  Child @ref tabParam cond ->
    getRef @(RdFrom (TRelDef sch ref)) True (demote @(RdCols (TRelDef sch ref)))
      tabParam cond
  Parent @ref cond ->
    getRef @(RdTo (TRelDef sch ref)) False (demote @(RdCols (TRelDef sch ref)))
      defTabParam cond
  UnsafeCond m -> m
  where
    getNot c
      | c == mempty = mempty
      | otherwise   = "not (" <> c <> ")"
    getBoolOp bo cc1 cc2
      | cc1 == mempty = cc2 -- so EmptyCond works both with &&& and |||
      | cc2 == mempty = cc1
      | otherwise = case bo of
        And -> cc1 <> " and " <> cc2
        Or -> "(" <> cc1 <> " or " <> cc2 <>  ")"
    getRef
      :: forall tab. CTabDef sch tab
      => Bool -> [(Text, Text)] -> TabParam sch tab -> Cond sch tab
      -> CondMonad Text
    getRef isChild cols tabParam cond = do
      tpp <- tabPref
      modify (+1)
      cnum <- get
      (tpc, condInt, ordInt, condExt) <- local (second (cnum <|))
        $ (,,,) <$> tabPref <*> convCond tabParam.cond
          <*> convOrd tabParam.order <*> convCond cond
      pure $ mkExists tpp tpc condInt ordInt condExt
      where
        mkExists tpp tpc cin oint cout
          = "exists (select 1 from (select * from " <> tn <> " " <> tpc
          <> " where "
          <> T.intercalate " and " (
              (\(ch,pr) -> tpc <> "." <> ch <> " = "
                <> tpp <> "." <> pr)
              . (if isChild then id else swap)
            <$> cols)
          <> (if T.null cin then "" else " and " <> cin)
          <> case tabParam of
            TabParam EmptyCond [] (LO Nothing Nothing) -> ""
            TabParam _ _ (convLO -> loTxt) ->
              (if T.null oint then "" else " order by " <> oint) <> loTxt
          <> ") " <> tpc
          <> (if T.null cout then "" else " where " <> cout)
          <> ")"
          where
            tn = qualName $ demote @tab

pgCond :: forall sch t . Int -> Cond sch t -> (Text, [SomeToField])
pgCond n cond = evalRWS (convCond cond) ("q", pure n) 0

pgOrd :: forall sch t. Int -> [OrdFld sch t] -> (Text, [SomeToField])
pgOrd n ord = evalRWS (convOrd ord) ("o", pure n) 0

pgDist :: forall sch t. Int -> Dist sch t -> (DistTexts, [SomeToField])
pgDist n dist = evalRWS (convDist dist) ("o", pure n) 0

withCondWithPath :: forall sch t r. (forall t'. Cond sch t' -> r) ->
  [Text] -> CondWithPath sch t -> Maybe r
withCondWithPath f p (CondWithPath @p' cond) = f cond <$ guard (p == demote @p')

withCondsWithPath :: forall sch t r. (forall t'. Cond sch t' -> r) ->
  [Text] -> [CondWithPath sch t] -> Maybe r
withCondsWithPath f p = join . L.find isJust . L.map (withCondWithPath f p)

cwp :: forall path -> forall sch t t1.
  (t1 ~ TabOnPath sch t path, ToStar path) => Cond sch t1 -> CondWithPath sch t
cwp p = CondWithPath @p

rootCond :: Cond sch t -> CondWithPath sch t
rootCond = cwp []

condByPath :: Int -> [Text] -> [CondWithPath sch t] -> (Text, [SomeToField])
condByPath num p = F.fold . withCondsWithPath (pgCond num) p

ordByPath :: Int -> [Text] -> [OrdWithPath sch t] -> (Text, [SomeToField])
ordByPath num p = F.fold . withOrdsWithPath (pgOrd num) p

distByPath :: Int -> [Text] -> [DistWithPath sch t] -> (DistTexts, [SomeToField])
distByPath num p =
  fromMaybe (emptyDistTexts, []) . withDistsWithPath (pgDist num) p

withOrdWithPath :: forall sch t r. (forall t'. [OrdFld sch t'] -> r) ->
  [Text] -> OrdWithPath sch t -> Maybe r
withOrdWithPath f p (OrdWithPath @p ord) = f ord <$ guard (p == demote @p)

withDistWithPath :: forall sch t r. (forall t'. Dist sch t' -> r) ->
  [Text] -> DistWithPath sch t -> Maybe r
withDistWithPath f p (DistWithPath @p dist) = f dist <$ guard (p == demote @p)

--
withOrdsWithPath :: forall sch t r . (forall t'. [OrdFld sch t'] -> r) ->
  [Text] -> [OrdWithPath sch t] -> Maybe r
withOrdsWithPath f p = join . L.find isJust . L.map (withOrdWithPath f p)

withDistsWithPath :: forall sch t r . (forall t'. Dist sch t' -> r) ->
  [Text] -> [DistWithPath sch t] -> Maybe r
withDistsWithPath f p = join . L.find isJust . L.map (withDistWithPath f p)

owp :: forall path -> forall sch t t'.
  (ToStar path, TabOnPath sch t path ~ t') =>
  [OrdFld sch t'] -> OrdWithPath sch t
owp p = OrdWithPath @p

rootOrd :: forall sch t. [OrdFld sch t] -> OrdWithPath sch t
rootOrd = owp []

dwp :: forall path -> forall sch t t'.
  (ToStar path, TabOnPath2 sch t path ~ '(t', 'RelMany)) =>
  Dist sch t' -> DistWithPath sch t
dwp p = DistWithPath @p

rootDist :: forall sch t. Dist sch t -> DistWithPath sch t
rootDist = dwp []

convPreOrd :: forall sch tab. [OrdFld sch tab] -> CondMonad [(Text, OrdDirection)]
convPreOrd = traverse processFld
  where
    processFld = \case
      OrdFld @fld od -> (, od) <$> qual @fld
      UnsafeOrd m -> m

renderOrd :: [(Text, OrdDirection)] -> Text
renderOrd = T.intercalate "," . fmap render
  where
    render (t, show' -> od) = t <> " " <> od <> " nulls last"

convOrd :: forall sch tab. [OrdFld sch tab] -> CondMonad Text
convOrd = fmap renderOrd . convPreOrd

data DistTexts = DistTexts
  { distinct :: Bool
  , distinctOn :: Text
  , orderBy :: Text }

emptyDistTexts :: DistTexts
emptyDistTexts = DistTexts { distinct = False, distinctOn = "", orderBy = "" }

convDist :: forall sch tab. Dist sch tab -> CondMonad DistTexts
convDist = \case
  Distinct -> pure $ DistTexts
    { distinct = True, distinctOn = mempty, orderBy = mempty }
  DistinctOn ofs -> convPreOrd ofs <&> \xs -> DistTexts
    { distinct = False
    , distinctOn = T.intercalate "," $ fst <$> xs
    , orderBy = renderOrd xs }
