module Database.PostgreSQL.Schema.Schema where

import Control.Monad
import Control.Monad.Catch
import Data.ByteString as BS hiding (readFile, writeFile)
import Data.Hashable
import Data.Semigroup ((<>))
import Data.Text (Text)
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DML.Condition
import Database.PostgreSQL.DML.Order
import Database.PostgreSQL.DML.Select
import Database.PostgreSQL.Schema.Catalog
import Database.PostgreSQL.Schema.Info
import Database.PostgreSQL.Simple


data ExceptionSch
  = ConnectException ByteString SomeException
  | GetDataException (Text, [SomeToField]) SomeException
  deriving Show

instance Exception ExceptionSch

getSchema :: Connection -> Text -> IO ([PgType], [PgClass], [PgRelation])
getSchema conn ns = do
  types <- catch (selectSch @PgCatalog @"pg_type" @PgType conn qpEmpty)
    ( throwM . GetDataException
      (selectText @PgCatalog @"pg_type" @PgType qpEmpty ) )
  classes <- catch (selectSch @PgCatalog @"pg_class" @PgClass conn qpClass)
    ( throwM . GetDataException
      (selectText @PgCatalog @"pg_class" @PgClass qpClass) )
  relations <- catch
    (selectSch @PgCatalog @"pg_constraint" @PgRelation conn qpRel)
    ( throwM . GetDataException
      (selectText @PgCatalog @"pg_constraint" @PgRelation qpRel) )
  pure (types, classes, relations)
  where
    qpClass = qpEmpty
      { qpConds =
        [ rootCond
          $ pparent @"class__namespace" (#nspname =? ns)
            &&& (pin @"relkind" (PgChar <$> "vr")) -- views & tables
        , cwp @'["attribute__class"] (#attnum >? (0::Int)) ]
      , qpOrds =
        [ owp @'["attribute__class"] [ascf @"attnum"] ] }
    qpRel = qpEmpty
      { qpConds =
        [rootCond $ pparent @"constraint__namespace" (#nspname =? ns)] }

getSchemaHash :: Connection -> Text -> IO Int
getSchemaHash conn = fmap hash . getSchema conn

getSchemaHash' :: ByteString -> Text -> IO Int
getSchemaHash' connStr ns = connectPostgreSQL connStr >>= flip getSchemaHash ns

updateSchemaHash :: ByteString -> Text -> FilePath -> IO Bool
updateSchemaHash connStr dbSchema file = do
  h <- getSchemaHash' connStr dbSchema
  s <- readFile file
  let
    s' = unlines $ setHash h <$> lines s
    -- check length to force close file. Where indirective...
    isChanged = Prelude.length s > 0 && s /= s'
  when isChanged $ writeFile file s'
  pure isChanged
  where
    setHash h s = case words s of
      ["hashSchema","=",x]
        | (fst <$> reads x) /= [h]  -> "hashSchema = " <> show h
      _                             -> s
