module Database.PostgreSQL.DML.Insert2 where

import Database.PostgreSQL.DB
-- import Database.PostgreSQL.Simple
import Database.Schema.Rec
import Data.String
import PgSchema.Util
import Database.Schema.ShowType (qualName)


insertText
  :: forall sch t r r' s. (IsString s, Monoid s, InsertReturning PG sch t r r')
  => s
insertText
  | null iChildren = "insert into " <> tn <> "(" <> fs <> ") values ("
    <> qs <> ")" <> " returning " <> rs
  | otherwise = "with t0 (_rid, " <> rs <> "," <> ")"
  where
    ir = getInsertRecord @PG @sch @t @r
    tn = fromText $ qualName ir.iTableName
    fs = intercalate' "," $ fromText . (.fpDbName) <$> iPlain
    qs = intercalate' "," $ "?" <$ iPlain
    iChildren = [x | IFieldTo x <- ir.iFields]
    iPlain = [x | IFieldPlain x <- ir.iFields]
    qFlds = (getQueryRecord @PG @sch @t @r').qFields
    -- qChildren = [x | QFieldTo  <- qFlds]
    -- qPlain = [x | QFieldPlain  <- qFlds]
    rs = intercalate' "," [ fromText qfp.fpDbName | (QFieldPlain qfp) <- qFlds]
