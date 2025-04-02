module Database.PostgreSQL.DML.Insert where

import Data.Bifunctor
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DB
import Database.PostgreSQL.DML.Insert.Types
import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util


insertSch :: forall sch t -> forall r r'. (ToRow r, FromRow r') =>
  (InsertReturning PG sch t r r', AllDmlPlain PG sch t r) =>
  Connection -> [r] -> IO [r']
insertSch sch t @r @r' conn = returning conn (insertText sch t r r')

insertSch_ :: forall sch t -> forall r. ToRow r =>
  (InsertNonReturning PG sch t r, AllDmlPlain PG sch t r) =>
  Connection -> [r] -> IO Int64
insertSch_ sch t @r conn = executeMany conn (insertText_ sch t r)

insertText :: forall sch t r r' -> InsertReturning  PG sch t r r' => Query
insertText sch t r r' = insertText_ sch t r <> " returning " <> fs'
  where
    qr' = getQueryRecord PG sch t r'
    fs' = fromText
      $ T.intercalate "," [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']

insertText_ :: forall sch t r -> CDmlRecord PG sch t r => Query
insertText_ sch t r = "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ir = getDmlRecord PG sch t r
    (fs,qs) = bimap inter inter
      $ unzip [ (ifp.fpDbName,"?") | (DmlFieldPlain ifp) <- ir.iFields]
    tn = fromText $ qualName ir.iTableName
    inter = fromText . T.intercalate ","
