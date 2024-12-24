{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS
import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Singletons
import Data.String
import Data.Text as T
import Data.Tuple
import Database.PostgreSQL.DB
import Database.PostgreSQL.DML.Select.Types
import Database.PostgreSQL.Simple hiding(In(..))
import Database.Schema.Def
import Database.Schema.Rec
import Database.Schema.ShowType
import GHC.TypeLits
import PgSchema.Util


selectSch
  :: forall sch tab r. (FromRow r, CQueryRecord PG sch tab r)
  => Connection -> QueryParam sch tab -> IO [r]
selectSch conn qp = let (q,c) = selectQuery @sch @tab @r qp in
  query conn q c

selectQuery
  :: forall sch tab r. CQueryRecord PG sch tab r
  => QueryParam sch tab -> (Query,[SomeToField])
selectQuery = first (fromString . unpack) . selectText @sch @tab @r

selectText
  :: forall sch tab r. CQueryRecord PG sch tab r
  => QueryParam sch tab -> (Text,[SomeToField])
selectText qp = evalRWS (selectM (getQueryRecord @PG @sch @tab @r)) (qr0 qp) qs0

qr0 :: QueryParam sch t -> QueryRead sch t
qr0 qrParam = QueryRead
  { qrCurrTabNum = 0
  , qrIsRoot = True
  , qrPath = []
  , qrParam }

qs0 :: QueryState
qs0 = QueryState
  { qsLastTabNum = 0
  , qsJoins = []
  , qsHasWhere = False
  , qsOrd = ""
  , qsLimOff = "" }

two :: (a,b,c) -> (a,b)
two (a,b,_c) = (a,b)

third :: (a,b,c) -> c
third (_a,_b,c) = c

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

selectM :: MonadQuery sch t m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  QueryRead {..} <- ask
  (fmap two -> fields) <- traverse fieldM queryFields
  joins <- gets $ L.reverse . qsJoins
  let
    (condText, condPars) =
      condByPath qrCurrTabNum (L.reverse qrPath) $ qpConds qrParam
    (ordText, ordPars) =
      ordByPath qrCurrTabNum (L.reverse qrPath) $ qpOrds qrParam
    sel
      | qrIsRoot =
        T.intercalate "," $ L.map (\(a,b) -> a <> " \"" <> b <> "\"") fields
      | otherwise = jsonPairing fields
    whereText
      | condText == mempty = ""
      | otherwise = " where " <> condText
    orderText
      | ordText == mempty = ""
      | otherwise = " order by " <> ordText
    qsLimOff = loByPath (L.reverse qrPath) $ qpLOs qrParam
  modify (\qs -> qs { qsHasWhere = whereText /= mempty })
  unless qrIsRoot $ modify (\qs -> qs { qsOrd = orderText, qsLimOff })
  tell $ condPars <> ordPars
  pure $ "select " <> sel
    <> " from " <> qualName tableName <> " t" <> show' qrCurrTabNum
    <> " " <> T.unwords joins
    <> whereText
    <> (if qrIsRoot then orderText else "")
    <> (if qrIsRoot then qsLimOff else "")

-- | return text for field, alias and expression to check is empty
-- (not obvious for FieldTo)
fieldM :: MonadQuery sch tab m => QueryField -> m (Text, Text, Text)
fieldM (FieldPlain _ dbname _) = do
  n <- asks qrCurrTabNum
  let val = "t" <> show' n <> "." <> dbname
  pure (val, dbname, val <> " is null")

fieldM (FieldFrom _ dbname QueryRecord{..} refs) = do
  QueryRead {..} <- ask
  modify \QueryState{qsLastTabNum, qsJoins} -> QueryState
    { qsLastTabNum = qsLastTabNum+1
    , qsJoins = joinText qrCurrTabNum (qsLastTabNum+1) : qsJoins
    , qsHasWhere = False
    , qsOrd = ""
    , qsLimOff = "" }
  n2 <- gets qsLastTabNum
  flds <- local
    (\qr -> qr{ qrCurrTabNum = n2, qrIsRoot = False, qrPath = dbname : qrPath })
    $ traverse fieldM queryFields
  let val = fldt flds
  pure (val, dbname, val <> " is null")
  where
    nullable = L.any (fdNullable . fromDef) refs
    joinText n1 n2 =
      outer <> "join " <> qualName tableName <> " t" <> show' n2
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

fieldM (FieldTo _ dbname rec refs) = do
  QueryRead{..} <- ask
  QueryState {qsLastTabNum, qsJoins} <- get
  modify (const $ QueryState (qsLastTabNum+1) [] False "" "")
  selText <- local
    (\qr -> qr
      { qrCurrTabNum = qsLastTabNum+1, qrIsRoot = False, qrPath = dbname : qrPath })
    (selectM rec)
  modify (\qs -> qs { qsJoins = qsJoins })
  QueryState{qsHasWhere, qsOrd, qsLimOff} <- get
  let
    val = "array(" <> selText <> " " <> (if qsHasWhere then "and" else "where")
      <> " " <> refCond (qsLastTabNum+1) qrCurrTabNum refs
      <> qsOrd <> qsLimOff <> ")"
  pure ("array_to_json(" <> val <> ")", dbname, val <> " = '{}'")

refCond :: Int -> Int -> [QueryRef] -> Text
refCond nFrom nTo = T.intercalate " and " . fmap compFlds
  where
    compFlds QueryRef {fromName, toName} =
      fldt nFrom fromName <> "=" <> fldt nTo toName
      where
        fldt n = (("t" <> show' n <> ".") <>)

withLOWithPath
  :: forall sch t r. (LO -> r) -> [Text] -> LimOffWithPath sch t -> Maybe r
withLOWithPath f path (LimOffWithPath @p lo) =
  guard (path == demote @p) >> pure (f lo)

withLOsWithPath
  :: forall sch t r. (LO -> r) -> [Text] -> [LimOffWithPath sch t] -> Maybe r
withLOsWithPath f path = join . L.find isJust . L.map (withLOWithPath f path)

lowp
  :: forall (path::[Symbol]) sch t. (ToStar path, TabPath sch t path)
  => LO -> LimOffWithPath sch t
lowp = LimOffWithPath @path

rootLO :: forall sch t. LO -> LimOffWithPath sch t
rootLO = lowp @'[]

convLO :: LO -> Text
convLO (LO ml mo) =
  maybe "" ((" limit " <>) . show') ml
   <> maybe "" ((" offset " <>) . show') mo

loByPath :: forall sch t. [Text] -> [LimOffWithPath sch t] -> Text
loByPath path = fromMaybe mempty . withLOsWithPath convLO path

runCond :: Int -> CondMonad a -> (a,[SomeToField])
runCond n x = evalRWS x ("q", pure n) 0

tabPref :: CondMonad Text
tabPref = asks \case
  (_, n :| []) -> "t" <> show' n
  (p, n :| (np : _)) -> "t" <> show' np <> p <> show' n

qual :: forall (fld :: Symbol). ToStar fld => CondMonad Text
qual = tabPref <&> (<> "." <> (demote @fld))

--
convCond :: forall sch t. Cond sch t -> CondMonad Text
convCond = \case
  EmptyCond -> pure mempty
  Cmp @n cmp v -> do
    tell [SomeToField v]
    qual @n <&> (<> " " <> showCmp cmp <> " ?")
  In @n (toList -> vs) -> do
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
      | cc1 == mempty = cc2
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

pgCond :: forall sch t. Int -> Cond sch t -> (Text, [SomeToField])
pgCond n cond = evalRWS (convCond cond) ("q", pure n) 0

pgOrd :: forall sch t. Int -> [OrdFld sch t] -> (Text, [SomeToField])
pgOrd n cond = evalRWS (convOrd cond) ("o", pure n) 0

withCondWithPath
  :: forall sch t r
  . (forall t'. Cond sch t' -> r)
  -> [Text] -> CondWithPath sch t -> Maybe r
withCondWithPath f path (CondWithPath @path' cond) =
  f cond <$ guard (path == demote @path')

withCondsWithPath
  :: forall sch t r
  . (forall t'. Cond sch t' -> r)
  -> [Text] -> [CondWithPath sch t] -> Maybe r
withCondsWithPath f path =
  join . L.find isJust . L.map (withCondWithPath f path)

cwp
  :: forall path sch t t1. (t1 ~ TabOnPath sch t path, ToStar path)
  => Cond sch t1 -> CondWithPath sch t
cwp = CondWithPath @path

rootCond :: forall sch t. Cond sch t -> CondWithPath sch t
rootCond = cwp @'[]

condByPath
  :: forall sch t. CSchema sch
  => Int -> [Text] ->[CondWithPath sch t] -> (Text, [SomeToField])
condByPath num path =
  fromMaybe mempty . withCondsWithPath (pgCond num) path

ordByPath
  :: forall sch t. CSchema sch
  => Int -> [Text] -> [OrdWithPath sch t] -> (Text, [SomeToField])
ordByPath num path =
  fromMaybe mempty . withOrdsWithPath (pgOrd num) path

withOrdWithPath
  :: forall sch t r
  . (forall t'. [OrdFld sch t'] -> r)
  -> [Text] -> OrdWithPath sch t -> Maybe r
withOrdWithPath f path (OrdWithPath @p ord) =
  f ord <$ guard (path == demote @p)
--
withOrdsWithPath
  :: forall sch t r
  . (forall t'. [OrdFld sch t'] -> r)
  -> [Text] -> [OrdWithPath sch t] -> Maybe r
withOrdsWithPath f path = join . L.find isJust . L.map (withOrdWithPath f path)

owp
  :: forall path sch t t'. (ToStar path, TabOnPath sch t path ~ t')
  => [OrdFld sch t'] -> OrdWithPath sch t
owp = OrdWithPath @path

rootOrd :: forall sch t. [OrdFld sch t] -> OrdWithPath sch t
rootOrd = owp @'[]

convOrd :: forall sch tab. [OrdFld sch tab] -> CondMonad Text
convOrd ofs = T.intercalate "," <$> traverse showFld ofs
  where
    showFld = \case
      OrdFld @fld (show' -> od) -> do
        fld <- qual @fld
        pure $ fld <> " " <> od <> " nulls last"
      SelFld @rel @fld @t cond ofs' (show' -> od) -> do
        modify (+1)
        n <- get
        prefPar <- tabPref
        local (second (n <|)) do
          condText <- convCond cond
          ordText <- convOrd ofs'
          fld <- qual @fld
          pref <- tabPref
          pure $ "(select " <> fld
            <> " from " <> qualName (demote @t) <> " " <> pref
            <> " where " <> T.intercalate " and " (condText : rels prefPar pref)
            <> (if T.null ordText then "" else " order by " <> ordText)
            <> " limit 1) " <> od <> " nulls last"
        where
          RelDef{..} = demote @(TRelDef sch rel)
          f | rdFrom == demote @t = swap
            | otherwise = id
          rels pp p = mkRel . f <$> rdCols
            where
              mkRel (cp,c) = pp <> "." <> cp <> "=" <> p <> "." <> c
