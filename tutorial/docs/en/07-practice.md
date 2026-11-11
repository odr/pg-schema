# In practice

In the examples so far we had one database schema, all relations were declared
as foreign keys, and the schema itself never changed. A real project is more
complicated: tables are spread over several PostgreSQL schemas, not every
relation is a foreign key, and the database structure changes with every
migration. In this chapter we look at how `pg-schema` copes with all this and
how to make schema generation part of the development process and of CI.

The examples of this chapter are in `tutorial/app/app/Tut/Practice.hs`, and the
generator is in `tutorial/app/generator/Main.hs`. Before the examples the
program recreates the schema and adds two users, Alice (`id = 1`) and Bob
(`id = 2`).

## What the generator describes

Which tables and relations go into `Sch.hs` is defined by the last argument of
`updateSchemaFile`, a value of type `GenNames`. Our generator has this one:

```haskell
--8<-- "app/generator/Main.hs:gen-names"
```

`schemas` lists the database schemas all of whose tables should be described.
If only some tables of a schema are needed, they can be listed in `tables` by
their full names, for example `["billing" ->> "invoices"]`. The generator finds
the relations between the selected tables by their foreign keys, and
`addRelations` can add relations that do not exist in the database. Let us take
all of this in turn.

## Several database schemas

Besides the schema `tut`, the tutorial database has a schema `audit` with a log
of project changes:

```sql
--8<-- "sql/00-create-schema.sql:audit"
```

Since both schemas are listed in `GenNames`, the table `audit.project_log` is
described in the same `Sch.hs` as the others. The annotation for it differs
from `MyAnn` of chapter 02 only in the database schema in the table name, so we
define the same kind of synonym for it and, like `MyAnn`, pass it to operations
as the first argument:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-audit-ann"
```

The foreign key `audit.project_log.project_id → tut.projects.id` is an ordinary
relation, and the fact that the tables are in different schemas changes
nothing. We can, for example, insert a project together with log entries in one
call of `insertJSON`:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-tree-types"
```

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-audit-insert"
```

```text
--8<-- "out/tutorial.txt:prac-audit-insert"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:prac-audit-insert-sql"
    ```

And the log can be read starting from the table `audit.project_log`, together
with the parent project from the schema `tut`:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-audit-select"
```

```text
--8<-- "out/tutorial.txt:prac-audit-select"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:prac-audit-select-sql"
    ```

## Relations that are not in the database

Sometimes tables are related by meaning, but the relation is not declared as a
foreign key. This happens with views (they have no foreign keys at all), with
partitioned tables, with old databases, or when a foreign key is left out on
purpose, for example because of the load. In the tutorial database the column
`task_events.author_id` is like this: it holds the `id` of a user, but has no
foreign key to `users`.

Such a relation can be described in the generator with `AddRelation`, as in
`GenNames` above: the name of the relation, the table it leads from, the table
it leads to, and the pairs of columns. After that `pg-schema` works with it just
as with a foreign key. In result types and in `qPath` its name goes through the
renamer as usual: `"taskEventsAuthor"`. Where a relation is given by its full
name together with the schema, as in `pchild` and `pparent` from chapter 04,
`_add` is written instead of the database schema: the generator puts all added
relations into this namespace. There is an example with `pparent` a bit below.

Let us add tasks with events. The column `author_id` is an ordinary column, and
we fill it with values; for one event we deliberately give the non-existent user
99:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-event-types"
```

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-events-insert"
```

Now let us read the events together with their authors:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-addrel"
```

```text
--8<-- "out/tutorial.txt:prac-addrel"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:prac-addrel-sql"
    ```

The column `author_id` allows `NULL`, so the author is `Maybe`, and the query
uses a `left outer join`. The event `(2, 1)` has no author because `author_id`
is empty, and the event `(2, 2)` because user 99 does not exist. The same
relation can be followed the other way, from a user to their events:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-addrel-children"
```

```text
--8<-- "out/tutorial.txt:prac-addrel-children"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:prac-addrel-children-sql"
    ```

And this is how events are selected by author with `pparent`. As in chapter 04,
the full name of the relation is convenient to write via a type synonym, only
the database schema here is `_add`:

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-add"
```

```haskell
--8<-- "app/app/Tut/Practice.hs:prac-addrel-parent"
```

```text
--8<-- "out/tutorial.txt:prac-addrel-parent"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:prac-addrel-parent-sql"
    ```

The documentation of `GenNames` (a fragment of it is in chapter 02) says "Be
careful!" about added relations. The database knows nothing about them and does
not check them: `author_id` may hold the `id` of a non-existent user, as with
the event `(2, 2)`, and the author is simply not found. The compiler checks that
the queries match the description of the relation, but it is up to you that the
description matches the data.

## When the schema changes

The most useful property of `pg-schema` shows itself when the database
structure changes. Take, for example, a migration that renames a column and
adds a mandatory one:

```sql
--8<-- "sql/migration-example.sql"
```

The column `code` is added without a default value, so this migration succeeds
only on an empty table, as in our tutorial database right after
`make tut-db-init`. A real database would need a default value or filling the
column, but for `pg-schema` this changes nothing: all that matters is that the
column is mandatory.

After the migration is applied, `Sch.hs` must be regenerated:

```bash
psql "$PG_CONN" -f tutorial/sql/migration-example.sql
cabal run tutorial:generator
```

The generator overwrites the file, and `git diff` shows what exactly changed in
the description of the database. The previous version is kept in `Sch.hs.bak`
and is overwritten on every generation, so this file should go into
`.gitignore`. If the description of the database has not changed, the file is
left untouched, and the output of the generator is deterministic, so there are
no spurious changes in `git diff`. Now it is enough to build the project, and
the compiler lists all the places the migration broke. In the tutorial project
there are 42 such errors, in five modules. Here are two of them. In chapter 03
we inserted projects without the column `code`:

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "projects"
  Missing mandatory fields: '["code"]
```

And in chapter 04 a condition used the column `priority`, which no longer
exists:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "priority" is not defined.

  Valid values are:
    Fields: project_id, seq, title, weight, payload, watchers.
    Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey.

  Your source or target type or renamer is probably invalid.
```

Without `pg-schema` each of these errors would show up only when the
corresponding query runs — in tests, if they cover it, or already in
production. Here the compiler immediately gives the full list of places to fix,
and until it is empty the program does not build.

To return the tutorial database to its initial state, run `make tut-db-init`
and `cabal run tutorial:generator` again.

This leads to an important practical rule: the generator should be a separate
program (in our project, a separate cabal executable) that does not depend on
`Sch.hs`. Otherwise, after a migration, until `Sch.hs` is regenerated, the
generator itself does not build. It must be run from the directory against
which the path to `Sch.hs` is given; in our case, from the root of the
repository.

## Checking in CI

It remains to make sure that `Sch.hs` in the repository does not fall behind
the database schema: someone may have added a migration and forgotten to
regenerate the file. The result of `updateSchemaFile` helps here: it returns
`True` if the file had to be created or changed. Our generator accepts the flag
`--check` and in that case exits with an error. Here it is in full (the first
argument of `updateSchemaFile` turns on verbose output):

```haskell
--8<-- "app/generator/Main.hs"
```

In CI it is enough to bring up a database (for example, a PostgreSQL service in
a container), apply all migrations to it with the same tool you use in the
project, set `PG_CONN` and run the generator with this flag. If `Sch.hs` is out
of date, the generator exits with an error, and the build fails. It also
overwrites the file, so when you run it on your own machine all that remains is
to look at `git diff` and commit the changes.

In the tutorial project all of this is done by a Makefile target:

```bash
make tut-schema-check
```

It recreates the tutorial database with the schema script and runs
`cabal run tutorial:generator -- --check`. In a real project, of course, the
database need not be recreated: the generator only needs a database with all
migrations applied.

You can also do it differently: not keep `Sch.hs` in the repository but generate
it in CI right before the build. Then no check is needed, but building without a
database becomes impossible. If a project has several databases, a separate
module is generated for each: `updateSchemaFile` is called several times with
different paths, module names and schema types.

## Summary

- `GenNames` defines the schemas, single tables and added relations.
- Tables from different database schemas are described in one `Sch.hs`, and
  relations between them work as usual.
- A relation without a foreign key is added with `AddRelation`, but the
  database does not check it.
- After a migration `Sch.hs` must be regenerated, and the compiler shows all
  the places to fix.
- In CI the generator with the check (`--check`) does not let you forget to
  regenerate.

## What next

This is the last chapter of the tutorial. The compiler messages you meet when
working with `pg-schema` are collected in the chapter
[Compiler errors](06-compiler-errors.md), and the rules and limitations in the
[reference](appendix-reference.md).
