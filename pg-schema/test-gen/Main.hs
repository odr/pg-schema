{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Exception (bracket)
import Data.Functor
import Data.ByteString.Char8 qualified as BS
import Database.PostgreSQL.Simple
import System.Environment
import PgSchema
import Prelude as P


main :: IO ()
main = do
  connStr <- maybe "dbname=schema_test" BS.pack <$> lookupEnv "PG_CONN"
  bracket (connectPostgreSQL connStr) close $ \conn -> do
    putStrLn "Setting up schema..."
    execute_ conn """
      CREATE EXTENSION IF NOT EXISTS citext SCHEMA public VERSION \"1.6\";
      CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\" SCHEMA public VERSION \"1.1\";
      DROP SCHEMA IF EXISTS test_schema CASCADE; 
      CREATE SCHEMA test_schema;
      SET search_path TO test_schema, public;
      CREATE TABLE base_converts 
        (cboolean boolean, cint4 int4, cfloat8 float8, cdate date        
        , ctime time, ctimestamp timestamp, ctimestamptz timestamptz, ctext text);
      create type color as enum ('red', 'green', 'blue');
      create table ext_converts 
        (ccitext citext, cbytea bytea, cjsonb jsonb, cjson json, cuuid uuid, ccolor color);
      create table base_arr_converts (cboolean boolean[], cint4 int4[]
        , cfloat8 float8[]
        , ctimestamptz timestamptz[] 
        , ctext text[]
        );
      create table ext_arr_converts 
        (ccitext citext[], cbytea bytea[], cjsonb jsonb[], cuuid uuid[], ccolor color[]);
      """

    putStrLn "Running generator..."
    void $ updateSchemaFile' False
      "pg-schema/test-pgs/Sch.hs"
      connStr
      "Sch" -- ^ haskell module name to generate
      "Sch" -- ^ name of generated haskell type for schema
      (GenNames ["test_schema"] [] []) -- ^ name of schemas in database
