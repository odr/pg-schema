module Database.PostgreSQL.DML.Insert where

import Data.Bifunctor
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DB
import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util


insertSch
  :: forall sch t r r'
  . ( InsertReturning PG sch t r r', ToRow r, FromRow r' )
  => Connection -> [r] -> IO [r']
insertSch conn = returning conn (insertText @sch @t @r @r')

insertSch_
  :: forall sch t r. (CDmlRecord PG sch t r, ToRow r)
  => Connection -> [r] -> IO Int64
insertSch_ conn = executeMany conn (insertText_ @sch @t @r)

insertText
  :: forall sch t r r'. (InsertReturning  PG sch t r r')
  => Query
insertText = insertText_ @sch @t @r <> " returning " <> fs'
  where
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromText
      $ T.intercalate "," [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']

insertText_ :: forall sch t r. CDmlRecord PG sch t r => Query
insertText_ = "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ir = getDmlRecord @PG @sch @t @r
    (fs,qs) = bimap inter inter
      $ unzip [ (ifp.fpDbName,"?") | (DmlFieldPlain ifp) <- ir.iFields]
    tn = fromText $ qualName ir.iTableName
    inter = fromText . T.intercalate ","

insertText'
  :: forall sch t r r'. (CDmlRecord PG sch t r, CQueryRecord PG sch t r')
  => Query
insertText' = insertText_ @sch @t @r <> " returning " <> fs'
  where
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromText
      $ T.intercalate "," [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']
