# pg-schema tutorial

[`pg-schema`](https://hackage.haskell.org/package/pg-schema) is a library for
working with PostgreSQL from Haskell in which the compiler checks your
queries. The schema is extracted once from a live PostgreSQL database into a
Haskell module, and from then on `select`, `insert`, `update` and `delete`
are built with compile-time checks. A typo in a column name, a relation to the
wrong table, a value of the wrong type, nested data of the wrong shape — all of
these become compile errors instead of errors at query time. And when the
database schema changes, you regenerate the module and the compiler shows every
place in the program that needs fixing.

`pg-schema` works on top of `postgresql-simple`: operations take an ordinary
`Connection` and run in ordinary transactions. Using the schema description,
`selectSch` reads data together with related rows by following foreign keys,
and `insertJSON` and `upsertJSON` write a whole tree of data — say, a project
together with its tasks — in one operation. Enums, `json` and `jsonb`, arrays
and composite keys are supported, and for whatever the library cannot express
there is a way out to raw SQL.

Here is what a typical query looks like:

```haskell
--8<-- "app/app/Tut/Why.hs:why-ann"

--8<-- "app/app/Tut/Why.hs:why-select"
```

`MyAnn` is the annotation shared by all operations: `Sch` is the generated
schema description, `"tut" ->> tab` is a table in the `tut` database schema,
`RenamerId` means that names in the program are the same as in the database,
and `5` limits the depth of nested relations. The field `tasks_project_id_fkey`
is named after the foreign key that links tasks to a project; with a renamer
you can give relations and columns the usual camelCase names instead. All this
is covered in detail in the chapter [Core workflow](02-core-workflow.md).

The result type defines which fields and relations to read: here, projects
with the list of their tasks (the fields are described with `PgTag`:
`"title" := Text`, and `:.` joins fields into a row). `qRoot` sets conditions
for the root table `projects`, and `qPath` for the branch of tasks. This is what
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
The schema description is taken from the database itself, so it cannot silently
drift from it. And a query is not written but built from the result type: you
describe which data you need and get one query that reads it. For complex trees
of data this is shorter and more reliable, and whatever cannot be expressed
this way can still be written as raw SQL. As for hasql, it is an alternative
not to `pg-schema` but to `postgresql-simple`, on top of which `pg-schema`
works.

## Requirements

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
