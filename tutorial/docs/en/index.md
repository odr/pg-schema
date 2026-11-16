# pg-schema tutorial

[`pg-schema`](https://hackage.haskell.org/package/pg-schema) is a library for
working with PostgreSQL from Haskell in which the compiler checks your
queries. Two ideas make it different:

- **The schema description is taken from the database itself**, so it cannot
  silently drift from it. A generator reads a live PostgreSQL database once
  and writes a Haskell module that describes its tables, columns, keys and
  relations at the type level.
- **An SQL query is not written but built in accordance with Haskell types and checked against the generated schema description.** You describe
  which data you need — which fields, and which related rows along foreign
  keys — and get one SQL query that reads exactly that. Inserts and updates
  work the same way, from the type of the input data.

A typo in a column name, a relation to the wrong table, a value of the wrong
type, nested data of the wrong shape — all of these become compile errors
instead of errors at query time. And when the database schema changes, you
regenerate the module and the compiler shows every place in the program that
needs fixing.

## How you work with pg-schema {#how-you-work-with-pg-schema}

The whole workflow has four steps. Every step links to the corresponding file
of the tutorial, so you can see what it looks like in a real project.

1. **Create the database.** The schema is defined in PostgreSQL as usual — by
   an SQL script or by migrations. pg-schema does not create or change tables.
   The tutorial schema:
   [`tutorial/sql/00-create-schema.sql`](https://github.com/odr/pg-schema/blob/master/tutorial/sql/00-create-schema.sql). Its
   comments show the names of the foreign keys: in pg-schema a relation
   between tables is referred to by the name of its FK constraint.
2. **Generate the schema description.** A small program — the generator —
   reads the PostgreSQL metadata and writes a Haskell module with the
   description of tables, columns, keys, relations and enums at the type
   level. Generator: [`tutorial/app/generator/Main.hs`](https://github.com/odr/pg-schema/blob/master/tutorial/app/generator/Main.hs),
   generated module: [`tutorial/app/app/Sch.hs`](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Sch.hs). The
   generator is re-run after every change to the database structure.
3. **Optionally, define your data types.** Ordinary records with
   `deriving Generic`, whose fields are named after columns and relations —
   for example, [`Project` and `Task`](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Tut/Wow.hs). No instances
   are needed. You do not have to define records at all: fields can be
   described right in the type of an operation with `PgTag`
   (`"title" := Text :. "status" := …`).
4. **Write queries with the EDSL, checked statically.** `selectSch`,
   `insertSch`, `insertJSON`, `upsertJSON`, `updateByCond`, `deleteByCond` and
   the others take an annotation with the generated schema and the base table.
   From the types of the input and the result, pg-schema builds the SQL, and
   the compiler checks every name, type, nullability and relation against
   the description from step 2. Examples: the chapters of this tutorial and
   their code in [`tutorial/app/app/Tut`](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Tut).

When the database changes, you repeat step 2, and the compiler points at every
query that no longer matches the database.

## An example {#an-example}

Here is a complete example: what is in the database, what is generated, and
what you write.

**The database (step 1).** The example uses two tables of the tutorial schema
`tut` — projects and their tasks — linked by a foreign key that PostgreSQL
named `tasks_project_id_fkey` (full script:
[`00-create-schema.sql`](https://github.com/odr/pg-schema/blob/master/tutorial/sql/00-create-schema.sql)):

```sql
create type project_status as enum ('draft', 'active', 'archived');

create table projects (
  id          bigserial primary key,
  owner_id    bigint not null references users(id),  -- projects_owner_id_fkey
  title       text not null,
  status      project_status not null default 'draft',
  ...
);

create table tasks (
  project_id  bigint not null references projects(id), -- tasks_project_id_fkey
  seq         integer not null,
  title       text not null,
  priority    integer not null default 100,
  ...
  primary key (project_id, seq)
);
```

**Generated (step 2).** You do not write or edit the module
[`Sch`](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Sch.hs): the generator creates it from the database. It
exports the schema type `Sch` and a Haskell type for every enum, here with the
constructors `Project_status_draft`, `Project_status_active` and
`Project_status_archived`.

**Written by you (steps 3 and 4).** This is the whole user code of the
example, including the extensions and the imports:

```haskell
--8<-- "app/app/Tut/Why.hs:why-header"
--8<-- "app/app/Tut/Why.hs:why-imports"

--8<-- "app/app/Tut/Why.hs:why-ann"

--8<-- "app/app/Tut/Why.hs:why-select"
```

Where the names come from:

| Name | Where from |
|------|------------|
| `Sch`, `Project_status_active` | generated module `Sch` |
| `Ann`, `RenamerId`, `->>`, `:=`, `selectSch`, `qRoot`, `qWhere`, `qPath`, `=?`, `<?` | library, module `PgSchema.DML` |
| `:.`, `Connection` | `postgresql-simple` (`:.` is re-exported by `PgSchema.DML`) |
| `MyAnn`, the result type, the query | your code |
| `"projects"`, `"title"`, `"status"`, `"priority"`, `"tasks_project_id_fkey"` | names from the database, checked by the compiler against `Sch` |

`MyAnn` is the annotation shared by all operations: `Sch` is the generated
schema description, `"tut" ->> tab` is a table in the `tut` database schema,
`RenamerId` means that names in the program are the same as in the database,
and `5` limits the depth of nested relations. The relation from projects to
their tasks is named after the foreign key, `tasks_project_id_fkey`; with a
renamer you can give relations and columns the usual camelCase names instead.
All this is covered in detail in the chapter [Core workflow](02-core-workflow.md).

Step 3 — defining data types — is optional, and this example skips it: the
result type is written right in the signature with `PgTag` fields
(`"title" := Text`, joined into a row with `:.`). Here it is projects with the
list of their tasks. Below, the same can be done with ordinary records.
`qRoot` sets conditions for the root table `projects`, and `qPath` for the
branch of tasks. This is what
the query returns on the tutorial data:

```text
--8<-- "out/tutorial.txt:why-select"
```

The whole result is read by one SQL query that `pg-schema` builds itself:

```sql
--8<-- "out/tutorial.txt:why-select-sql"
```

And if you mistype the column name as `"priorty"`, the program does not
compile, and the compiler lists all the columns of the table:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "priorty" is not defined.

  Valid values are:
    Fields: project_id, seq, title, priority, payload, watchers.
    Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey.

  Your source or target type or renamer is probably invalid.
```

Such queries can easily be made as complex as you need. The type defines the
shape of the result — which tables, through which relations and with which
fields — and conditions, ordering and limits can be added to any of its
branches. From this description one SQL query is built that reads the whole
tree at once. In practice such queries join twenty tables and more. Writing
such SQL by hand is very hard, and gluing it together from pieces in code is
hard to maintain. Here complexity and safety work together: the bigger the
query, the more valuable it is that every name and every type in it is checked
by the compiler.

Finally, reading and writing rows needs no instances at all — neither
`FromRow`/`ToRow` nor `FromJSON`/`ToJSON`. Deriving `Generic` or describing the
fields with `PgTag` is enough. (Instances are only needed for your own value
types inside columns, for example a type stored in `jsonb`.) This makes
parameterized data types easy to use. Large, similar structures often need
different fields in different parts of a program: say, a list of projects needs
only the titles, while a detailed card also needs the owner and the tasks.
Instead of two nearly identical types you can have one, parameterized by a
context, and give the fields that are not needed in a context the type `()`
(`If` comes from `Data.Type.Bool`, and `==` from `Data.Type.Equality`):

```haskell
--8<-- "app/app/Tut/Why.hs:why-param"
```

The two calls of `selectSch` in `whyParam` look the same: `pg-schema` decides
what to read from the result type, and that is given by the signature. Fields
of type `()` are not read at all. For `Project 'List` the query boils down to
selecting titles from the single table `projects`, with no join to users and
no subquery for tasks. For `Project 'Details` both the owner (one row: the
foreign key points from the project to the user) and the list of tasks come
from the database. Nothing extra is fetched, and there is still a single type.

## Performance

Speed has been a concern of `pg-schema` from the start. What matters most is
the number of round-trips to the database: a tree of data is read by one query
and written by one operation instead of many separate queries to related
tables, each of them a separate exchange with the server. In practice this is
where the main gain comes from.

Type-level checks are not free for the compiler, and this has received a lot of
attention too. With a schema of about 50 tables and queries joining twenty
tables and more, compile times stay acceptable. A large database can be
described by several `Sch` schemas. The load is most visible in HLS: it uses
more memory than in projects without type-level computation, but this is
acceptable for work on real projects.

## Other libraries

Among the libraries for type-safe database access from Haskell there are, for
example, opaleye, beam and persistent. In them the database schema is described
in Haskell code, and queries are built from expressions of an embedded
language, which lets you write almost any query. `pg-schema` works differently.
As described at the top, the schema description is taken from the
database itself, and a query is built from the result type. For complex trees
of data this is shorter and more reliable, and whatever cannot be expressed
this way can still be written as raw SQL. As for hasql, it is an alternative
not to `pg-schema` but to `postgresql-simple`, on top of which `pg-schema`
works.

## Requirements {#requirements}

- GHC ≥ 9.10: the library uses visible type arguments
  (`selectSch (MyAnn "users") …`). You do not need to enable
  `RequiredTypeArguments` in your own code for this. The tutorial is checked
  with GHC 9.12.4 (see `cabal.project` at the repository root).
- PostgreSQL, with `psql`/`createdb` on the `PATH`.
- GHC extensions. The examples use `GHC2024`, which already includes
  `DataKinds` and `TypeApplications`. You also need:
    - `OverloadedStrings` — for string values (`Text`) in data and
      conditions. Names of tables, fields and relations (`"title"`,
      `MyAnn "users"`) do not depend on it: they are type-level literals
      (`Symbol`);
    - `BlockArguments` — for `qRoot do …` and `qPath "…" do …`;
    - `TypeFamilies` and `UndecidableInstances` — for your own renamer;
    - `DuplicateRecordFields` — if several record types have fields with the
      same name (for example, `title` of a project and of a task).

## Contents

1. [Quick start](01-wow.md) — from an empty database to inserting and reading a tree of data.
2. [Core workflow](02-core-workflow.md) — schema generation, first inserts and selects.
3. [Single-table DML](03-flat-dml.md) — insert, update, upsert and delete.
4. [Select](04-select.md) — queries with conditions, relations between tables.
5. [Tree DML](05-tree-dml.md) — writing a tree of data in one operation: `insertJSON`, `upsertJSON`, `updateJSON`.
6. [Compiler errors](06-compiler-errors.md) — typical compiler messages and what to do about them.
7. [In practice](07-practice.md) — several database schemas, relations without foreign keys, migrations and a CI check.

The [reference](appendix-reference.md) collects the rules that the chapters
show by example, and the appendix [Data types](appendix-types.md) covers the
mapping between PostgreSQL and Haskell types and how to add your own types.

## Before you start

You need a running PostgreSQL, GHC with cabal (see the requirements above) and
a clone of the `pg-schema` repository: it contains the tutorial examples. In
your own project, the `pg-schema` package from Hackage is enough. Everything
else — creating the tutorial database, generating the schema and running the
examples — is described in the chapter [Quick start](01-wow.md). The examples
are in `tutorial/app`, the SQL scripts in `tutorial/sql`, and the chapters in
`tutorial/docs`.
