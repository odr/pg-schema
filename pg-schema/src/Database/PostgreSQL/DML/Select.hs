{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.DML.Select where

import Control.Monad.RWS
import Data.Bifunctor
import Data.List as L
import Data.String
import Data.Text as T
import Data.Text.Lazy (toStrict)
import Database.PostgreSQL.DB
import Database.PostgreSQL.DML.Condition
import Database.PostgreSQL.DML.Limit
import Database.PostgreSQL.DML.Order
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Rec
import Formatting


data QueryParam sch t = QueryParam
  { qpConds :: !([CondWithPath sch t])
  , qpOrds  :: !([OrdWithPath sch t])
  , qpLOs   :: !([LimOffWithPath sch t]) }

qpEmpty :: forall sch t. QueryParam sch t
qpEmpty = QueryParam [] [] []

data QueryRead sch t = QueryRead
  { qrSchema     :: !Text
  , qrCurrTabNum :: !Int
  , qrIsRoot     :: !Bool
  , qrPath       :: !([Text])
  , qrParam      :: !(QueryParam sch t) }

data QueryState = QueryState
  { qsLastTabNum :: !Int
  , qsJoins      :: !([Text])
  , qsWhere      :: !Bool
  , qsOrd        :: !Text
  , qsLimOff     :: !Text }
  deriving Show

type MonadQuery sch t m =
  (CSchema sch, MonadRWS (QueryRead sch t) [SomeToField] QueryState m)

selectSch
  :: forall sch tab r. (FromRow r, CQueryRecord PG sch tab r)
  => Connection -> QueryParam sch tab -> IO [r]
selectSch conn cond = let (q,c) = selectQuery @sch @tab @r cond in
  query conn q c

selectQuery
  :: forall sch tab r. CQueryRecord PG sch tab r
  => QueryParam sch tab -> (Query,[SomeToField])
selectQuery = first (fromString . unpack) . selectText @sch @tab @r

selectText
  :: forall sch tab r. CQueryRecord PG sch tab r
  => QueryParam sch tab -> (Text,[SomeToField])
selectText cond = evalRWS (selectM (getQueryRecord @PG @sch @tab @r))
  (QueryRead (schemaName @sch) 0 True [] cond) (QueryState 0 [] False "" "")

jsonPairing :: [(Text, Text)] -> Text
jsonPairing fs = "jsonb_build_object(" <> T.intercalate "," pairs <> ")"
  where
    pairs = L.map (\(a,b) -> "'" <> b <> "'," <> a) fs

selectM :: MonadQuery sch t m => QueryRecord -> m Text
selectM QueryRecord {..} = do
  QueryRead {..} <- ask
  fields <- traverse fieldM queryFields
  joins <- L.reverse . qsJoins <$> get
  let
    (condText, pars) =
      condByPath qrCurrTabNum (L.reverse qrPath) $ qpConds qrParam
    sel
      | qrIsRoot    =
        T.intercalate "," $ L.map (\(a,b) -> a <> " \"" <> b <> "\"") fields
      | otherwise = jsonPairing fields
    j = T.intercalate " " joins
    whereText
      | condText == mempty = ""
      | otherwise       = " where " <> toStrict condText
    ordText
      | t == mempty = ""
      | otherwise = " order by " <> t
      where
        t = ordByPath qrCurrTabNum (L.reverse qrPath) $ qpOrds qrParam
    loText = loByPath (L.reverse qrPath) $ qpLOs qrParam
  modify (\qs -> qs { qsWhere = whereText /= mempty })
  unless qrIsRoot $ modify (\qs -> qs { qsOrd = ordText, qsLimOff = loText })
  tell pars
  pure $ sformat fmt sel qrSchema tableName qrCurrTabNum j whereText
    (if qrIsRoot then ordText else "") (if qrIsRoot then loText else "")
  where
    fmt = "select " % stext
      % " from " % stext % "." % stext % " t" % int % " " % stext
      % stext % stext % stext

fieldM :: MonadQuery sch tab m => QueryField -> m (Text, Text)
fieldM (FieldPlain name dbname _) = do
  n <- qrCurrTabNum <$> ask
  pure $ (sformat fmt n dbname, name)
  where
    fmt = "t" % int % "." % stext

fieldM (FieldFrom name QueryRecord {..} refs) = do
  QueryRead {..} <- ask
  modify (\QueryState{..} -> QueryState
    (qsLastTabNum+1)
    (joinText qrSchema qrCurrTabNum (qsLastTabNum+1) : qsJoins)
    False "" "")
  n2 <- qsLastTabNum <$> get
  f <- jsonPairing
    <$> local (\qr -> qr
      { qrCurrTabNum = n2, qrIsRoot = False, qrPath = name : qrPath })
        (traverse fieldM queryFields)
  pure (f, name)
  where
    joinText schName n1 n2 =
      sformat fmt outer schName tableName n2 (refCond n1 n2 refs)
      where
        fmt = stext % "join " % stext % "." % stext % " t" % int
          % " on " % stext
        outer
          | L.any (fdNullable . fromDef) refs = "left outer "
          | otherwise = ""

fieldM (FieldTo name rec refs) = do
  QueryRead {..} <- ask
  (QueryState ltn joins _ _ _) <- get
  modify (const $ QueryState (ltn+1) [] False "" "")
  selText <- local
    (\qr -> qr
      { qrCurrTabNum = ltn+1, qrIsRoot = False, qrPath = name : qrPath })
    (selectM rec)
  modify (\qs -> qs { qsJoins = joins })
  (QueryState _ _ isWhere ordText loText) <- get
  pure (sformat fmt selText (if isWhere then "and" else "where")
    (refCond (ltn+1) qrCurrTabNum refs) ordText loText, name)
  where
    fmt = "array_to_json(array("%stext%" "%text%" "%stext%stext%stext%"))"

refCond :: Int -> Int -> [QueryRef] -> Text
refCond nFrom nTo = T.intercalate " and " . L.map compFlds
  where
    fld = "t" % int % "." % stext
    compFlds QueryRef {..} = sformat (fld % "=" % fld) nFrom fromName nTo toName

--
-- {-
-- "
-- select rel.relname, rel.relkind
--   , array_to_json(array(
--     select
--       jsonb_build_object(
--         'name', c.attname
--         , 'num', c.attnum
--         , 'null', not c.attnotnull
--         , 'typ', json_build_object(
--           'name', t.typname
--           , 'typns', tns.nspname
--           , 'typ', t.typtype
--           , 'cat', t.typcategory
--           )
--       )
--       from pg_attribute c
--         join pg_type t on c.atttypid = t.oid
--         left outer join pg_namespace tns on t.typnamespace = tns.oid
--       where c.attrelid = rel.oid
--         and c.attnum > 0
--       order by c.attnum
--     )) cols
--     , array_to_json(array(
--       select
--         jsonb_build_object(
--           'typ', con.contype
--           , 'key', array_to_json(con.conkey)
--         )
--         from pg_constraint con
--         where con.contype in ('p','u')
--           and con.conrelid = rel.oid))
--   from pg_catalog.pg_class rel
--     join pg_catalog.pg_namespace nsp on rel.relnamespace = nsp.oid
--   where nsp.nspname = 'tinkoff'
--         and rel.relkind in ('r','v')
-- "
-- -}
