# Quick start

This chapter is a short example of working with `pg-schema`. We will work with
data in two related tables at once: projects and their tasks. We write projects
together with their tasks in one operation and read them back with one query,
and the compiler checks the names of tables, fields and relations before the
program even runs.

The chapter follows the four steps of the [workflow](index.md#how-you-work-with-pg-schema):

1. [create the database](#step-1-the-database);
2. [generate the schema description](#step-2-schema-generation);
3. [define the data types](#step-3-application-data-types-optional) (optional);
4. [write queries](#step-4-queries), which the compiler checks against the
   description.

If you only want to run the examples, two commands from the repository root
are enough (you need PostgreSQL where you may create databases, and GHC with
cabal — see the [requirements](index.md#requirements)):

```bash
export PG_CONN='dbname=tutorial'
```

```bash
make tut-run
```

`make tut-run` creates the database, runs the generator and then all the
examples of the tutorial, and saves their output to `tutorial/out/tutorial.txt`.

??? note "Preparation in detail: the connection string, separate commands, the first build"

    All commands are run from the root of a clone of the `pg-schema`
    repository, where `cabal.project` and the `Makefile` are. The schema
    generator and the examples application read the database connection string
    from the `PG_CONN` environment variable. The tutorial database is called
    `tutorial`, so for a local PostgreSQL `dbname=tutorial` is enough. If your
    server needs a host, a user or a password, add them to the same string, for
    example `dbname=tutorial host=localhost user=postgres`. The `make` commands
    use the same variable.

    `make tut-run` is made of three steps that can also be run separately:

    ```bash
    make tut-db-init               # create the database and the schemas
    cabal run tutorial:generator   # regenerate tutorial/app/app/Sch.hs
    cabal run tutorial:tutorial    # run the examples of all chapters
    ```

    `make tut-db-init` creates the `tutorial` database if it does not exist yet
    and runs the script `tutorial/sql/00-create-schema.sql`. The script starts
    by dropping the schemas, so you can run it as often as you like. To make
    sure that the tables exist, run `psql "$PG_CONN" -c "\dt tut.*"`.

    The examples program runs the examples of all chapters in order, and the
    examples of this chapter come first. Before them it recreates the `tut`
    schema itself, so the output does not depend on previous runs. You still
    need `make tut-db-init` once: it creates the database itself, and the
    generator needs the schema before the examples run.

    The first build from scratch takes a while, because cabal builds all the
    dependencies. It is convenient to start it in advance in another terminal
    and keep reading: `cabal build tutorial:tutorial`.

## Step 1. The database {#step-1-the-database}

`pg-schema` does not create tables: the schema is defined in PostgreSQL as
usual. The tutorial schema `tut` is created by the script
[`tutorial/sql/00-create-schema.sql`](https://github.com/odr/pg-schema/blob/master/tutorial/sql/00-create-schema.sql). In this
chapter we need two of its tables — `projects` and `tasks` — and `users`,
because every project has an owner:

```sql
--8<-- "sql/00-create-schema.sql:wow-tables"
```

Note the names of the foreign keys in the comments. We did not name the
constraints, so PostgreSQL named them by default: the table, the columns and
the suffix `_fkey`. In `pg-schema` a relation is referred to by exactly this
name: below, the field `tasks_project_id_fkey` of a project will hold the list
of its tasks. If you prefer other names, give the constraints names in the
database (`constraint … foreign key …`) or use a renamer, which we will meet
in the next chapter.

??? note "The whole script `00-create-schema.sql`"

    ```sql
    --8<-- "sql/00-create-schema.sql"
    ```

## Step 2. Schema generation {#step-2-schema-generation}

To check our queries, the compiler needs to know how the database is built.
This description is created by the generator, and it must be run after every
change to the database structure. The generator is a small program,
[`tutorial/app/generator/Main.hs`](https://github.com/odr/pg-schema/blob/master/tutorial/app/generator/Main.hs): it lists which
database schemas to describe and calls `updateSchemaFile`, which reads the
PostgreSQL metadata and writes it to the module
[`tutorial/app/app/Sch.hs`](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Sch.hs):

```haskell
--8<-- "app/generator/Main.hs:gen-main"
```

```bash
cabal run tutorial:generator
```

The file `Sch.hs` is already in the repository, and the generator rewrites it
only if the database description has changed. If you forget to run the
generator after changing the structure, the compiler checks queries against an
outdated description: new columns and tables are unknown to it, while queries
to dropped or renamed columns compile and fail only at run time. `Sch.hs`
describes the tables, their fields and relations at the type level. You do not
need to read it to use the library; we look at its contents in the chapter
[Core workflow](02-core-workflow.md). For now it is enough to know that the
examples import this module.

## Step 3. Application data types (optional) {#step-3-application-data-types-optional}

This step is optional: instead of records, the fields can be listed right in
the type of an operation with `PgTag`, as in the [introduction](index.md#an-example).
With records, though, the code is easier to read.

The examples of this chapter are in
[`tutorial/app/app/Tut/Wow.hs`](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Tut/Wow.hs). The project
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

## Step 4. Queries {#step-4-queries}

Now we can work with the data. All examples of this step are
functions in `IO` that take a `postgresql-simple` connection.

### Inserting data

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

### Reading data

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

### SQL without running the query

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

### :thinking_face: What if…

Let us see what happens if we get a name wrong. Suppose we made a typo in the
type `Task` and named the field `titel` instead of `title`. The program does not
compile, and the compiler says in which table it looked for the name and which
names the table has:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "titel" is not defined.

  Valid values are:
    Fields: project_id, seq, payload, priority, title, watchers.
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
    Fields: project_id, seq, payload, priority, title, watchers.
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
