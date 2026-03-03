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
      DROP SCHEMA IF EXISTS test_pgs CASCADE; 
      CREATE SCHEMA test_pgs;
      SET search_path TO test_pgs, public;
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

      -- Lookup table for dimensions (used by root via dim_a_id, dim_b_id for "two child lists by same FK" tests).
      create table dim (
        id        serial primary key,
        code      text not null,
        name      text not null,
        constraint dim_code_uq unique (code)
      );

      -- Root of hierarchy; two FKs to dim for distinct roles (dim_a, dim_b).
      create table root (
        id         serial primary key,
        code       text not null,
        grp        integer not null,
        name       text not null,
        created_at timestamptz not null default now(),
        dim_a_id   integer,
        dim_b_id   integer,
        constraint root_code_grp_uq unique (code, grp),
        constraint root_dim_a_fk foreign key (dim_a_id) references dim (id) on delete restrict,
        constraint root_dim_b_fk foreign key (dim_b_id) references dim (id) on delete restrict
      );

      -- Simple child (single-column PK/FK).
      create table mid1 (
        id       serial primary key,
        root_id  integer not null,
        pos      integer not null,
        flag     boolean not null,
        sort_key integer not null,
        payload  text,
        constraint mid1_root_fk foreign key (root_id) references root (id) on delete cascade
      );

      -- Child with composite PK and single-column FK to root.
      create table mid2 (
        root_id   integer not null,
        seq       integer not null,
        kind      text not null,
        flag      boolean not null,
        priority  integer not null,
        payload   jsonb,
        constraint mid2_pk primary key (root_id, seq),
        constraint mid2_root_fk foreign key (root_id) references root (id) on delete cascade
      );

      -- Leaf with composite PK and composite FK to mid2.
      create table leaf (
        root_id    integer not null,
        seq        integer not null,
        leaf_no    integer not null,
        value      float4 not null,
        category   text,
        created_at timestamptz not null default now(),
        constraint leaf_pk primary key (root_id, seq, leaf_no),
        constraint leaf_mid2_fk foreign key (root_id, seq) references mid2 (root_id, seq) on delete cascade
      );

      -- Table for array types (nullable elements, no 2D arrays).
      create table arrays (
        id             serial primary key,
        root_id        integer,
        dates_nullable date[],
        jsons          jsonb[],
        constraint arrays_root_fk foreign key (root_id) references root (id) on delete cascade
      );
      """

    putStrLn "Running generator..."
    void $ updateSchemaFile' False
      "pg-schema/test-pgs/Sch.hs"
      connStr
      "Sch" -- ^ haskell module name to generate
      "Sch" -- ^ name of generated haskell type for schema
      (GenNames ["test_pgs"] [] []) -- ^ name of schemas in database
