-- tutorial/sql/00-create-schema.sql
drop schema if exists tut cascade;
create schema tut;
set search_path to tut, public;

create type project_status as enum ('draft', 'active', 'archived');

create table users (
  id          bigserial primary key,
  name        text not null,
  email       text unique,
  created_at  timestamptz not null default now()
);

create table projects (
  id          bigserial primary key,
  owner_id    bigint not null references users(id) on delete restrict,
  title       text not null,
  status      project_status not null default 'draft',
  tags        text[] not null default '{}',         -- PgArr демонстрация
  created_at  timestamptz not null default now()
);

-- composite PK для демонстрации upsert по PK
create table tasks (
  project_id  bigint not null references projects(id) on delete cascade,
  seq         integer not null,
  title       text not null,
  priority    integer not null default 100,
  payload     jsonb,
  watchers    bigint[] not null default '{}',       -- еще PgArr
  primary key (project_id, seq)
);

create table task_events (
  project_id  bigint not null,
  seq         integer not null,
  event_no    integer not null,
  kind        text not null,
  meta        jsonb,
  created_at  timestamptz not null default now(),
  primary key (project_id, seq, event_no),
  foreign key (project_id, seq)
    references tasks(project_id, seq)
    on delete cascade
);
