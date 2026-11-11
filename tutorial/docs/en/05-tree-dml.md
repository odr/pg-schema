# Tree DML

In the chapter [Single-table DML](03-flat-dml.md) each operation worked with one
table. But data often comes as a whole tree: a project with its tasks, and tasks
with their events. Writing such a tree piece by piece is awkward: you have to
insert the project, learn its `id`, put it into the tasks, insert them, and so
on. `pg-schema` can write the whole tree in one operation. There are three
functions for this: `insertJSON` inserts new data, `upsertJSON` inserts new data
and updates existing data, and `updateJSON` only updates. Each has a variant
with an underscore (`insertJSON_` and so on) that returns nothing from the
database.

All three functions work the same way, as we already saw in the chapter
[Quick start](01-wow.md). From the types of the input and the result,
`pg-schema` generates a temporary plpgsql function, creates it in the
database, calls it, passing the whole tree as one parameter of type `jsonb`, and
drops it. These are three commands, and their number does not change however
many rows the tree has. If no transaction is open yet (this is seen from the
connection state), all of it runs in a separate transaction, so the tree is
written entirely or not at all.

The examples of this chapter are in `tutorial/app/app/Tut/TreeDml.hs`. Before
them the program recreates the schema and adds one user with `id = 1`, the
owner of all projects.

## Insert: insertJSON

We describe the input data with types. Child rows are fields with a list of
rows, named after the foreign key. As in chapters 01 and 04, this is the name of
the constraint in the database (`tasks_project_id_fkey`,
`task_events_project_id_seq_fkey`) passed through the renamer `MyRenamer`, so we
write it in camelCase:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-types"
```

The task type has no `project_id` column, and the event type has no
`project_id` and `seq` columns, although they are all mandatory. Their values
come from the parent row: `insertJSON` first inserts the project, gets its `id`
and puts it into the tasks, and then does the same with the events. So
`insertJSON` requires all mandatory columns in the input except those that come
from the relation with the parent and those that have a default value. If a
mandatory column is missing, the program does not compile, just as with
`insertSch` in chapter 03.

The trees themselves are convenient to build with the function `rowsOf`. It is
similar to `named` from chapter 03, but `named` gets the field names as a list
of strings, while `rowsOf` takes both the names and the types of the fields from
the row type, which is passed to it as `rowsOf @ProjectIn`. So values can be
written without names and without type annotations, and child rows simply as
nested lists.

As with single-table operations, the result type defines what to return after
the insert. Let us return the keys of all inserted rows, on all three levels:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-keys"
```

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-insert"
```

```text
--8<-- "out/tutorial.txt:tree-insert"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:tree-insert-sql"
    ```

The result type may contain any columns of the tables, including those that
were not in the input, like the `id` of a project. But the branches of the
result must be among the branches of the input: you can return tasks only if
you inserted them. This, like the other rules for the result type, is checked
at compile time.

`insertJSON_` does the same but returns nothing from the database. Instead of a
function it creates a temporary procedure and returns to the program only the
generated SQL. If you do not need the result, this saves a little work: the
database does not have to build a `jsonb` answer, and the program does not have
to parse it.

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-insert-nr"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:tree-insert-nr-sql"
    ```

## Insert or update: upsertJSON

`insertJSON` only inserts. If a row with the same key already exists,
PostgreSQL reports a uniqueness violation, and the whole operation is rolled
back. When data may be either new or already existing, use `upsertJSON`. It
decides what to do with the rows for each level of the tree (that is, for each
table) from the fields of its type, and it decides this at compile time:

- if the type has a key but not all mandatory columns, the rows are updated by
  that key (`update`);
- if it has both a key and all mandatory columns, the rows are inserted, and on
  a key conflict updated (`insert … on conflict … do update`);
- if it has all mandatory columns but no key, the rows are simply inserted
  (`insert`);
- if it has neither a key nor all mandatory columns, the program does not
  compile: there is nothing to do with such a row.

Mandatory columns here are the same as for `insertJSON`: `NOT NULL` without a
default, except those that come from the parent. The key is the primary key,
or, if its columns are not in the type, a unique key all of whose columns are in
the type. Unique keys over `NOT NULL` columns are considered first, and keys
with nullable columns only if the table has none of the former. Columns that
come from the parent count too. After the parent row, its child rows are
processed the same way. Let us change project 1: rename its first task, add an
event to it, and add a new task to the project:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-patch-types"
```

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-upsert"
```

```text
--8<-- "out/tutorial.txt:tree-upsert"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:tree-upsert-sql"
    ```

The root here is only the key of the project, so the project is updated by
`id` (there is nothing to change in it, but it must be found to get to the
tasks). The tasks have the key `(project_id, seq)`, where `project_id` comes
from the project, and all mandatory columns: we passed `title`, and `priority`
has a default. So for the tasks an `insert … on conflict` runs: task 1 already
existed, and its title changed, while task 3 did not exist and was inserted. On
a conflict only the columns that are in the type are updated, so the priority of
task 1 stayed the same. The new event was inserted the same way.

Note the result type `PatchOut`: the root in it is wrapped in `Maybe`, but the
tasks are not. The project is updated by key (`update`) and may not be found,
while the tasks go through `insert … on conflict`, after which the task row
always exists. So `Maybe` is needed only where an `update` runs. If a row is
not found, its place in the result holds `Nothing`, and its child rows are not
processed:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-upsert-missing"
```

```text
--8<-- "out/tutorial.txt:tree-upsert-missing"
```

There is no project 999, and the task "Ghost" was not inserted. Where `Maybe`
is required in the result of `upsertJSON` and where it is forbidden is
summarized in a table in the [reference](appendix-reference.md#returning).

## Update: updateJSON

`updateJSON` never inserts anything: it updates each row of the tree by its key.
So a key is needed on every level, but mandatory columns are not. Let us set
project 2 to the status `active`, change the priority of its task 1, and try to
update the non-existent task 9 and the non-existent project 999:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-update-types"
```

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-update"
```

```text
--8<-- "out/tutorial.txt:tree-update"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:tree-update-sql"
    ```

The result shows the fate of each row. Project 2 was found, and in its list of
tasks the first one changed, while task 9 is replaced by `Nothing`. There is no
project 999, and it is replaced by `Nothing` too. The lists of the result come
in the same order as the input, so each `Nothing` is easy to match with what was
not found. Any row may be missing, so in the result type of `updateJSON` both
the root and the elements of child lists must be wrapped in `Maybe`.

If some row of the tree has no key, there is nothing to update it by, and the
program does not compile. Let us remove, for example, `seq` from the task type:

```
• We can't upsert data because for table NameNS "tut" "tasks"
  not all mandatory fields are present and no full primary or
  unique key is covered by record fields.
  Missing mandatory fields: ["seq", "title"]
  Possible keys: '[["project_id", "seq"]]
```

The message is shared by `upsertJSON` and `updateJSON`, so it talks about
upsert and mandatory columns. For `updateJSON` the last line matters: the list
of keys, one of which must be entirely in the type. The column `project_id`
comes from the project, so for the tasks `seq` is missing.

## Transactions

As we said, each of these operations opens a transaction itself if none is open
yet, and on an error nothing partly written remains. If a transaction is already
open, the operation runs inside it and is subject to it: when the outer
transaction is rolled back, everything the operation wrote is rolled back too.
This lets you combine several operations into one transaction. In the next
example we insert a project with the same `id` twice inside `withTransaction`
from `postgresql-simple`:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-tx"
```

```text
--8<-- "out/tutorial.txt:tree-tx"
```

The second insert violated uniqueness, and `withTransaction` rolled back the
whole transaction, including the first, successful insert. There is no project
"Tx demo" in the database. To count projects we used an aggregate from
chapter 04:

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-count"
```

## Summary

- `insertJSON` inserts a tree in one operation, and columns that come from the
  relation with the parent need not be given.
- The branches of the result must be among the branches of the input; the
  columns may be any.
- `upsertJSON` chooses `update`, `insert … on conflict` or `insert` for each
  level from the fields of its type.
- `updateJSON` never inserts and requires a key on every level.
- Rows that are not found give `Nothing` in their place in the result.
- The operations run in a transaction and are subject to an outer transaction.

## What next

We have covered all the main operations of `pg-schema`: reading and changing
data both in one table and in several related ones. The compiler messages we met
along the way are collected in the chapter
[Compiler errors](06-compiler-errors.md), and the rules we saw by example in the
[reference](appendix-reference.md).
