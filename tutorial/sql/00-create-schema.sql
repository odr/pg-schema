-- tutorial/sql/00-create-schema.sql
--
-- Database schema of the pg-schema tutorial (step 1 of the workflow).
-- The generator (tutorial/app/generator/Main.hs) reads this structure from
-- the live database and writes tutorial/app/app/Sch.hs.
--
-- Names of foreign keys matter: in pg-schema a relation between tables is
-- referred to by the name of its FK constraint (a field of a record type or
-- a `qPath` step). None of the constraints below is named explicitly, so
-- PostgreSQL gives them default names:
--   <table>_<column1>_<column2>..._fkey
-- The resulting names are written next to each FK. You can also name a
-- constraint yourself: `constraint my_name foreign key ... references ...`.
--
-- The script drops the schemas first, so it can be run again at any time.
drop schema if exists audit cascade;
drop schema if exists tut cascade;
create schema tut;
set search_path to tut, public;

-- --8<-- [start:wow-tables]
-- enum -> Haskell type `PGEnum Sch ("tut" ->> "project_status")` with
-- constructors Project_status_draft | Project_status_active | Project_status_archived
create type project_status as enum ('draft', 'active', 'archived');

create table users (
  id          bigserial primary key,
  name        text not null,                         -- NOT NULL: plain Haskell type
  email       text unique,                           -- nullable: Maybe in Haskell
  manager_id  bigint references users(id),           -- nullable self-FK: users_manager_id_fkey
  created_at  timestamptz not null default now()
);

create table projects (
  id          bigserial primary key,
  owner_id    bigint not null
              references users(id) on delete restrict, -- FK: projects_owner_id_fkey
  title       text not null,
  status      project_status not null default 'draft',
  tags        text[] not null default '{}',          -- PgArr demo
  created_at  timestamptz not null default now()
);

-- composite PK to demonstrate upsert by PK
create table tasks (
  project_id  bigint not null
              references projects(id) on delete cascade, -- FK: tasks_project_id_fkey
                                                         -- (project -> its tasks)
  seq         integer not null,
  title       text not null,
  priority    integer not null default 100,
  payload     jsonb,
  watchers    bigint[] not null default '{}',        -- one more PgArr
  primary key (project_id, seq)
);
-- --8<-- [end:wow-tables]

create table task_events (
  project_id  bigint not null,
  seq         integer not null,
  event_no    integer not null,
  kind        text not null,
  meta        jsonb,
  author_id   bigint,                                -- no FK: the generator adds the relation
                                                     -- task_events_author_id_fkey (chapter 07)
  created_at  timestamptz not null default now(),
  primary key (project_id, seq, event_no),
  -- composite FK: task_events_project_id_seq_fkey
  foreign key (project_id, seq)
    references tasks(project_id, seq)
    on delete cascade
);

-- second schema: a log of project changes (chapter 07)
-- --8<-- [start:audit]
create schema audit;

create table audit.project_log (
  id          bigserial primary key,
  project_id  bigint not null
              references tut.projects(id) on delete cascade, -- FK: audit.project_log_project_id_fkey
  message     text not null,
  client_ip   inet,                                 -- a type with no built-in mapping (appendix "Data types")
  created_at  timestamptz not null default now()
);
-- --8<-- [end:audit]
