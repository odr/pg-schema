{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DerivingVia #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS
import Control.Monad
import Data.Bifunctor
import Data.Foldable as F
import Data.Functor
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import Data.Singletons
import Data.String
import Data.Text as T
import Data.Tuple
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple hiding(In(..))
import Database.Schema.Def
import Database.Schema.Rec
import Database.Schema.ShowType
import GHC.Generics
import GHC.TypeLits
import PgSchema.Util
import Prelude as P
import Prelude.Singletons as SP hiding (Any)


data QueryRead sch t = QueryRead
  { qrCurrTabNum  :: Int
  , qrPath        :: [Text]
  , qrParam       :: QueryParam sch t }

data ParentInfo = ParentInfo
  { piRelDbName   :: Text
  , piFromNum     :: Int
  , piToNum       :: Int
  , piParentTab   :: NameNS
  , piRefs        :: [Ref' Text]
  , piPath        :: [Text] }
  deriving Show

data QueryState = QueryState
  { qsLastTabNum  :: Int
 , qsParents     :: [ParentInfo] }
  deriving Show

type MonadQuery sch t m = (MonadRWS (QueryRead sch t) [SomeToField] QueryState m)

selectSch :: forall sch tab r.
  (FromRow r, CRecordInfo sch tab r) =>
  Connection -> QueryParam sch tab -> IO ([r], (Text,[SomeToField]))
selectSch conn (selectText @sch @tab @r -> (sql,fs)) =
  trace' ("\n\n" <> T.unpack sql <> "\n\n" <> P.show fs <> "\n\n")
  $ (,(sql,fs)) <$> query conn (fromString $ unpack sql) fs

selectQuery :: forall sch tab r. (CRecordInfo sch tab r) =>
  QueryParam sch tab -> (Query,[SomeToField])
selectQuery = first (fromString . unpack) . selectText @sch @tab @r

selectText :: forall sch t r. (CRecordInfo sch t r) =>
  QueryParam sch t -> (Text,[SomeToField])
selectText qp = evalRWS (selectM "" (getRecordInfo @sch @t @r)) (qr0 qp) qs0

qr0 :: QueryParam sch t -> QueryRead sch t
qr0 qrParam = QueryRead
  { qrCurrTabNum = 0 , qrPath = [] , qrParam }

qs0 :: QueryState
qs0 = QueryState { qsLastTabNum = 0, qsParents = [] }

two :: (a,b,c) -> (a,b)
two (a,b,_) = (a,b)

third :: (a,b,c) -> c
third (_,_,c) = c

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = mapMaybe (\(a,b) -> if "$EmptyField" `T.isSuffixOf` b then Nothing
      else Just $ "'" <> b <> "'," <> a) fs

newtype TextI (s::Symbol) = TextI { unTextI :: Text}

instance KnownSymbol s => Semigroup (TextI s) where
  TextI a <> TextI b =
    TextI $ T.intercalate (demote @s) $ L.filter (not . T.null) [a,b]

instance KnownSymbol s => Monoid (TextI s) where mempty = TextI mempty

selectM :: MonadQuery sch t m => Text -> RecordInfo -> m Text
selectM refTxt ri = do
  QueryRead {..} <- ask
  (fmap two -> flds) <- traverse fieldM ri.fields
  -- qsParents are collected "in depth" (it is ok for joins etc) but in reverse order
  parents <- gets
    $ fmap (\p -> p { piPath = L.reverse p.piPath}) . L.reverse . qsParents
  let
    basePath = L.reverse qrPath
    (unTextI -> condText, condPars) = F.fold $ L.reverse
      $ mapMaybe (\(CondWithPath @path cond) -> let p = demote @path in
        if
          | not (basePath `L.isPrefixOf` p) -> Nothing
          | p == basePath -> Just $ first TextI $ pgCond qrCurrTabNum cond
          | otherwise -> L.find ((p ==) . (basePath <>) . (.piPath)) parents
            <&> \pari -> first (TextI @" and ") $ pgCond pari.piToNum cond
        ) qrParam.qpConds
    (unTextI -> ordText, ordPars) = F.fold $ L.reverse
      $ mapMaybe (\(OrdWithPath @path ord) -> let p = demote @path in
        if
          | not (basePath `L.isPrefixOf` p) -> Nothing
          | p == basePath -> Just $ pgOrd qrCurrTabNum ord
          | otherwise -> L.find ((p ==) . (basePath <>) . (.piPath)) parents
            <&> \pari -> pgOrd pari.piToNum ord
        ) qrParam.qpOrds
    (distTexts, distPars) = F.fold $ L.reverse
      $ mapMaybe (\(DistWithPath @path dist) -> let p = demote @path in
        if
          | not (basePath `L.isPrefixOf` p) -> Nothing
          | p == basePath -> Just $ pgDist qrCurrTabNum dist
          | otherwise -> L.find ((p ==) . (basePath <>) . (.piPath)) parents
            <&> \pari -> pgDist pari.piToNum dist
        ) qrParam.qpDistinct
    sel
      | L.null basePath =
        T.intercalate "," $ L.map (\(a,b) -> a <> " \"" <> b <> "\"") flds
      | otherwise = jsonPairing flds
    whereText = let conds = L.filter (not . T.null) [refTxt, condText] in
      if L.null conds then mempty else " where " <> T.intercalate " and " conds
    orderText
      | T.null ordText && T.null distTexts.orderBy.unTextI = ""
      | otherwise = " order by " <> T.intercalate ","
        (L.filter (not . T.null) [distTexts.orderBy.unTextI, ordText])
    distinctText
      | distTexts.distinct.getAny && T.null distTexts.distinctOn.unTextI = " distinct "
      | T.null distTexts.distinctOn.unTextI = ""
      | otherwise = " distinct on (" <> distTexts.distinctOn.unTextI <> ") "
    qsLimOff = loByPath (L.reverse qrPath) $ qpLOs qrParam
    groupByText
      | P.null aggrs || P.null others = ""
      | otherwise = " group by " <> T.intercalate ","
        (L.nub $ others >>= \fi -> case fi.fieldKind of
          RFPlain{} -> [fi.fieldDbName]
          RFToHere _ refs -> refs <&> \ref -> ref.toName
          RFFromHere _ refs -> refs <&> \ref -> ref.fromName
          _ -> [])
      where
        (aggrs, others) = L.partition
          (\fld -> case fld.fieldKind of { RFAggr{} -> True; _ -> False })
          ri.fields
  tell $ distPars <> condPars <> distPars <> ordPars
  pure $ "select "
    <> distinctText
    <> sel
    <> " from " <> qualName ri.tabName <> " t" <> show' qrCurrTabNum
    <> " " <> T.unwords (joinText <$> trace' (show' parents) parents)
    <> whereText
    <> groupByText
    <> orderText
    <> qsLimOff

-- | return text for field, alias and expression to check is empty
-- (not obvious for FieldTo)
fieldM :: MonadQuery sch tab m => FieldInfo RecordInfo -> m (Text, Text, Text)
fieldM fi = case fi.fieldKind of
  RFEmpty s -> pure ("null", s, "true")
  RFPlain {} -> do
    n <- asks qrCurrTabNum
    let val = "t" <> show' n <> "." <> fi.fieldDbName
    pure (val, fi.fieldDbName, val <> " is null")
  RFAggr _ fname _ -> do
    n <- asks qrCurrTabNum
    let val = fname <> "(" <> "t" <> show' n <> "." <> fi.fieldDbName <> ")"
    pure case fname of
      "count" -> ("count(*)", fi.fieldName, " false")
      _ -> (val, fi.fieldName, val <> " is null")
  RFFromHere ri refs -> do
    QueryRead {..} <- ask
    modify \QueryState{qsLastTabNum = (+1) -> n2, qsParents} -> QueryState
      { qsLastTabNum = n2
      -- , qsJoins = joinText qrCurrTabNum n2 : qsJoins
      , qsParents = ParentInfo
        { piRelDbName = fi.fieldDbName
        , piFromNum   = qrCurrTabNum
        , piToNum     = n2
        , piParentTab = ri.tabName
        , piRefs      = refs
        , piPath      = fi.fieldDbName : qrPath } : qsParents }
    n2 <- gets qsLastTabNum
    (flds, pars) <- listen $ local
      (\qr -> qr{ qrCurrTabNum = n2, qrPath = fi.fieldDbName : qrPath })
      $ traverse fieldM ri.fields
    val <- if L.any (fdNullable . fromDef) refs
      then do
        tell pars
        pure $ "case when " <> T.intercalate " and " (third <$> flds)
          <> " then null else " <> jsonPairing (two <$> flds) <> " end"
      else pure $ jsonPairing $ two <$> flds
    pure (val, fi.fieldDbName, val <> " is null")
  RFToHere ri refs -> do
    QueryRead{..} <- ask
    QueryState {qsLastTabNum = (+1) -> tabNum, qsParents} <- get
    modify (const $ QueryState tabNum [])
    selText <- local
      (\qr -> qr { qrCurrTabNum = tabNum, qrPath = fi.fieldDbName : qrPath })
      $ selectM (refCond tabNum qrCurrTabNum refs) ri
    modify (\qs -> qs { qsParents = qsParents })
    let
      val = "array(" <> selText <> ")"
    pure ("array_to_json(" <> val <> ")", fi.fieldDbName, val <> " = '{}'")

joinText :: ParentInfo -> Text
joinText ParentInfo{..} =
  outer <> "join " <> qualName piParentTab <> " t" <> show' piToNum
    <> " on " <> refCond piFromNum piToNum piRefs
  where
    outer
      | hasNullable piRefs = "left outer "
      | otherwise = ""

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
  (ToStar path, TabPath sch t path
  , Snd (TabOnPath2 sch t path) ~ RelMany) => LO -> LimOffWithPath sch t
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
    tell [SomeToField $ PgArr vs]
    qual @n <&> (<> " = any(?::" <> qualName (demote @(TFldDef sch t n)).fdType <> "[])")
  InArr @n vs -> do
    tell [SomeToField $ PgArr vs]
    qual @n <&> (<> " = any(?::" <> qualName (demote @(TFldDef sch t n)).fdType <> "[])")
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
      (tpc, condInt, TextI ordInt, condExt) <- local (second (cnum <|))
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

pgOrd :: forall sch t. Int -> [OrdFld sch t] -> (TextI ",", [SomeToField])
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

ordByPath :: Int -> [Text] -> [OrdWithPath sch t] -> (TextI ",", [SomeToField])
ordByPath num p = F.fold . withOrdsWithPath (pgOrd num) p

distByPath :: Int -> [Text] -> [DistWithPath sch t] -> (DistTexts, [SomeToField])
distByPath num p = F.fold . withDistsWithPath (pgDist num) p

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

renderOrd :: [(Text, OrdDirection)] -> TextI ","
renderOrd = foldMap (TextI . render)
  where
    render (t, show' -> od) = t <> " " <> od <> " nulls last"

convOrd :: forall sch tab. [OrdFld sch tab] -> CondMonad (TextI ",")
convOrd = fmap renderOrd . convPreOrd

data DistTexts = DistTexts
  { distinct :: Any
  , distinctOn :: TextI ","
  , orderBy :: TextI "," }
  deriving Generic
  deriving (Semigroup, Monoid) via (Generically DistTexts)

convDist :: forall sch tab. Dist sch tab -> CondMonad DistTexts
convDist = \case
  Distinct -> pure $ mempty { distinct = Any True }
  DistinctOn ofs -> convPreOrd ofs <&> \xs -> mempty
    { distinctOn = foldMap (TextI . fst) xs
    , orderBy = renderOrd xs }
