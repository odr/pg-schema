# Quick start

This chapter is a short example of working with `pg-schema`. We create a
database schema, connect it to a project, and write and read some data. We will
work with data in two related tables at once: projects and their tasks. We
write projects together with their tasks in one operation and read them back
with one query, and the compiler checks the names of tables, fields and
relations before the program even runs.

## Preparation

You need a clone of the `pg-schema` repository, a running PostgreSQL where you
may create databases, and GHC with cabal (version requirements are in the
[introduction](index.md)). All commands are run from the repository root, where
`cabal.project` and the `Makefile` are. The schema generator and the examples
application read the database connection string from the `PG_CONN` environment
variable. The tutorial database is called `tutorial`, so for a local PostgreSQL
this string is enough:

```bash
export PG_CONN='dbname=tutorial'
```

If your server needs a host, a user or a password, add them to the same
string, for example `dbname=tutorial host=localhost user=postgres`. The `make`
commands below use the same variable.

Now create the database and the tutorial schema `tut`:

```bash
make tut-db-init
```

This command creates the `tutorial` database if it does not exist yet and runs
the script `tutorial/sql/00-create-schema.sql`. The script starts by dropping
the `tut` schema, so you can run it as often as you like and get a clean
database each time. To make sure that the tables `users`, `projects`, `tasks`
and `task_events` exist, run:

```bash
psql "$PG_CONN" -c "\dt tut.*"
```

The first build of the project from scratch takes a while, because cabal builds
all the dependencies. It is convenient to start it in advance in another
terminal and keep reading: `cabal build tutorial:tutorial`.

## Schema generation

To check our queries, the compiler needs to know how the database is built.
This description is created by the generator, and it must be run after every
change to the database structure. The generator is a small program,
`tutorial/app/generator/Main.hs`: it calls `updateSchemaFile`, reads the
PostgreSQL metadata and writes it to the module `tutorial/app/app/Sch.hs`:

```bash
cabal run tutorial:generator
```

The file `tutorial/app/app/Sch.hs` is already in the repository, and the
generator rewrites it only if the database description has changed. If you
forget to run the generator after changing the structure, the compiler checks
queries against an outdated description: new columns and tables are unknown to
it, while queries to dropped or renamed columns compile and fail only at run
time. `Sch.hs` describes the tables of the `tut` schema, their fields and
relations at the type level. We look at its contents in detail in the chapter
[Core workflow](02-core-workflow.md); for now it is enough to know that the
examples import this module.

Now you can run the examples themselves:

```bash
cabal run tutorial:tutorial
```

The program runs the examples of all chapters in order, and the examples of
this chapter come first. Before them the program recreates the `tut` schema
itself, so the output does not depend on previous runs, and you can run it as
often as you like. You still need `make tut-db-init`: it creates the database
itself, and the generator needs the schema before the examples run. Generating
the schema and running the examples can also be done with a single command,
`make tut-run`: it also runs `tut-db-init` and saves the output to
`tutorial/out/tutorial.txt`. Below we go step by step through what the examples
of this chapter do and what they print.

## Application data types

The examples of this chapter are in `tutorial/app/app/Tut/Wow.hs`. The project
is built with `GHC2024`, which already enables `TypeApplications` and
`ScopedTypeVariables`, and the module enables a few more extensions:

```haskell
--8<-- "app/app/Tut/Wow.hs:extensions"
```

`BlockArguments` lets us write queries as `qRoot do …`, and
`DuplicateRecordFields` is needed because both a project and a task have a
`title` field. All `pg-schema` functions for working with data are in the
module `PgSchema.DML`, and the generated database description is imported from
`Sch`:

```haskell
--8<-- "app/app/Tut/Wow.hs:imports"
```

`pg-schema` works on top of `postgresql-simple`, and every operation takes an
ordinary `Connection`. All examples below run in `IO` inside the function
`wow conn`, and the connection `conn` is created by the application's `main`
from the `PG_CONN` string.

Every operation in `pg-schema` starts with an annotation `Ann`. It tells which
table we work with and how to match Haskell names with database names. To avoid
repeating it, we define a synonym:

```haskell
--8<-- "app/app/Tut/Wow.hs:ann"
```

The apostrophe in `'Ann` is there because the annotation is a type-level value
(that is why `DataKinds` is enabled). `Sch` is the generated schema, and `->>`
joins the name of the database schema with the name of the table. The number
`3` limits the depth of nested relations: it is needed mostly so that types with
mutual or cyclic relations do not unfold forever, and here it is more than
enough. `RenamerId` means that names are not converted at all: the fields of our
types are named exactly like the columns in the database. Later we will see how
a renamer lets us write names in the usual Haskell camelCase.

The data itself is described by ordinary record types, for which deriving
`Generic` is enough:

```haskell
--8<-- "app/app/Tut/Wow.hs:types"
```

Almost all fields here correspond to columns of the `projects` and `tasks`
tables. The exception is the field `tasks_project_id_fkey`. This is the name
PostgreSQL gave by default to the foreign key `tasks.project_id → projects.id`
(the name is made of the table, the column and the suffix `_fkey`). Seeing this
name, `pg-schema` finds the relation in the schema description and sees that the
key leads from `tasks` to `projects`. So for a project the field holds not a
column value but a list of child rows from `tasks`. We did not include the
`payload` and `watchers` columns of `tasks` in the type: a query uses only the
columns that are in the type. The type of the `status` field also comes from the
schema: `PGEnum Sch ("tut" ->> "project_status")` is the type the generator
created from the `project_status` enum in the database, and its values are
called `Project_status_draft`, `Project_status_active` and
`Project_status_archived`. The fields `id` and `seq` in this chapter's types
have the same names as functions from `Prelude`, but this does no harm: we
never call the fields ourselves, `pg-schema` works with them via `Generic`.

## Inserting data

Every project must have an owner: the column `projects.owner_id` is mandatory
and refers to the `users` table. So first we add one user. A type with the
single field `name` is enough, and to learn which `id` the database assigned to
the new user, we also describe a type with that field:

```haskell
--8<-- "app/app/Tut/Wow.hs:owner-type"
```

```haskell
--8<-- "app/app/Tut/Wow.hs:wow-owner"
```

```text
--8<-- "out/tutorial.txt:wow-owner"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:wow-owner-sql"
    ```

Note that `TutAnn "users"` is a type, yet it is passed as an ordinary first
argument. The functions of `pg-schema` are declared with a visible type
argument (that is why GHC 9.10 or newer is required), so the annotation can be
written right in the call, without `@`.

`insertSch` inserts rows into one table. As with other `pg-schema` functions,
the types define the query: the columns for `insert` come from the type of the
input data (`Owner`), and the columns for `returning` from the type of the
result (`UserKey`). That is why the query is exactly `insert into
tut.users(name) values (?) returning id`. We inserted one user and immediately
matched the result against the pattern `[UserKey ownerId]`.

Now we have `ownerId`, and we can insert two projects together with their
tasks. One call of `insertJSON` is enough:

```haskell
--8<-- "app/app/Tut/Wow.hs:wow-insert"
```

`insertJSON` returns data of the inserted rows, and the result type defines
which columns to return. We are interested in the keys of the projects and
their tasks, so we describe them with separate types:

```haskell
--8<-- "app/app/Tut/Wow.hs:key-types"
```

The example prints the keys of the inserted rows:

```text
--8<-- "out/tutorial.txt:wow-insert"
```

Rows are inserted into two tables, but the number of queries does not depend
on that. From the data type, `insertJSON` generates a temporary plpgsql function
that can insert projects together with their tasks. First it creates the
function in the database (`create function pg_temp.__ins …`), then calls it
(`select pg_temp.__ins(?)`), passing all the data as one parameter of type
`jsonb`, and then drops it. All three commands run in one transaction.
`insertJSON` returns the text of the function as the second element of the pair
(`sqlIns`), and you can look at it here:

??? note "The function created by `insertJSON`"

    ```sql
    --8<-- "out/tutorial.txt:wow-insert-sql"
    ```

Note also that we never gave `project_id` for the tasks: the function takes it
from the project it has just inserted.

## Reading data

Now let us read the data back. We describe the query separately, so that in the
next section we can look at its SQL without running it. The type
`QueryParamAnn` in the signature means "parameters of a query that starts from
the base table of the annotation", here `projects`. The query itself touches
several tables: through `qPath` it reaches the tasks too.

```haskell
--8<-- "app/app/Tut/Wow.hs:query"
```

`qRoot` sets conditions for the root table `projects`: we only want active
projects. Inside `qPath` the conditions apply to the branch of tasks: we take
tasks with a priority below 50 and order them by `seq`. The operators `=?` and
`<?` compare with a query parameter for "equal" and "less than", and `ascf`
orders by a field ascending. The type of the literal `50` is given explicitly
because a numeric literal can be of any numeric type, while the value
`Project_status_active` has only one type.

The name in `qPath` is the same relation name as the field
`tasks_project_id_fkey` of the type `Project`. The compiler catches a typo in
this name, but the conditions of the branch apply to the result field:

!!! warning "Conditions without a branch in the result"
    If the result type has no `tasks_project_id_fkey` field, tasks are not
    read, and the conditions from `qPath` silently do not get into the query.
    There is no compile error in this case.

Let us run the query and read the result into the same type `Project` that we
used for inserting:

```haskell
--8<-- "app/app/Tut/Wow.hs:wow-select"
```

```text
--8<-- "out/tutorial.txt:wow-select"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:wow-select-sql"
    ```

The project "Mobile app" is not in the result because its status is `draft`,
and the project "Website" has no task "Nice to have" because its priority is
100. Besides the rows, `selectSch` returned the query text and its parameters.
The parameters come in the order in which the `?` marks appear in the query
text, so the condition on tasks comes first. The text shows that the projects
together with their tasks were read by one SQL query: the tasks of each project
are gathered into a JSON array right inside it.

## SQL without running the query

Sometimes you want not to run a query but only to see what SQL it produces: in
a REPL, while debugging, or in a test that has no database. That is what
`selectText` is for. It takes the same annotation, result type and query
description as `selectSch`, but does not touch the database and returns the
query text and its parameters right away:

```haskell
--8<-- "app/app/Tut/Wow.hs:wow-select-text"
```

```text
--8<-- "out/tutorial.txt:wow-select-text"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:wow-select-text-sql"
    ```

This is the same query that `selectSch` ran in the previous section. The result
type is given explicitly here (`@Project`), because we get no rows, and the
compiler has nothing to infer it from.

## :thinking_face: What if…

Let us see what happens if we get a name wrong. Suppose we made a typo in the
type `Task` and named the field `titel` instead of `title`. The program does not
compile, and the compiler says in which table it looked for the name and which
names the table has:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "titel" is not defined.

  Valid values are:
    Fields: project_id, seq, title, priority, payload, watchers.
    Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey.

  Your source or target type or renamer is probably invalid.
```

The type `Task` itself knows nothing about the database. The check happens
where the type is used together with an annotation, so the compiler points at
each such call: `insertJSON`, `selectSch` and `selectText`.

Names in query conditions are checked the same way. If you write `"priorty"`
instead of `"priority"` in `activeProjects`, the message is almost the same:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "priorty" is not defined.

  Valid values are:
    Fields: project_id, seq, title, priority, payload, watchers.
    Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey.

  Your source or target type or renamer is probably invalid.
```

Note that the name is looked up in the table `tasks`, although the query starts
from `projects`. Inside `qPath "tasks_project_id_fkey"` the compiler already
knows that we followed the relation to tasks and checks conditions against
their fields. In plain SQL such a typo would surface only when the query runs.

## Summary

- The generator reads a live database and creates the module `Sch.hs` with a
  description of tables, columns and relations; it must be regenerated after
  schema changes.
- Data is described with ordinary records, and a field named after a foreign
  key links a project with its tasks.
- `insertJSON` inserts projects together with their tasks in one operation, and
  `selectSch` reads them with one query.
- `qRoot` sets conditions for the root table, `qPath` for a branch.
- `selectText` shows the SQL of a query without touching the database.
- Typos in names are caught by the compiler.

## What next

We have seen the whole path from an empty database to reading related data, but
took a lot on faith. In the chapter [Core workflow](02-core-workflow.md) we look
at what exactly is generated in `Sch.hs`, how the annotation `Ann` and renamers
work, and which other errors the compiler catches. Inserting, updating and
deleting data are covered in the chapters [Single-table DML](03-flat-dml.md)
and [Tree DML](05-tree-dml.md), and queries with conditions in the chapter
[Select](04-select.md).
