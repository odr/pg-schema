# Single-table DML

In this chapter we look at how to change data with `pg-schema`: insert, update
and delete rows of one table, and update or insert rows "by key". Operations
that touch several related tables in one query are covered in the chapter
[Tree DML](05-tree-dml.md).

As in the previous chapter, we use the annotation `MyAnn` with the renamer
`MyRenamer`, so fields can be named in camelCase, and we describe data with
`PgTag`. All examples of this chapter are in `tutorial/app/app/Tut/FlatDml.hs`
and run after the examples of chapter 02, on the same data.

For working with one table, `pg-schema` has five groups of operations:
`insertSch` inserts rows, `updateByCond` updates the rows that match a
condition, `deleteByCond` deletes them, and `updateByKey` and `upsertByKey`
update or insert rows found by a primary or unique key. Every operation except
delete has a variant with an underscore at the end of its name (`insertSch_`,
`updateByCond_` and so on) that returns nothing from the database. As with
reading, types define the SQL of each operation: the columns we write come from
the type of the input data, and the columns in `returning` from the type of the
result.

## Insert

We have already met `insertSch` and `insertSch_` in the previous chapter. Let us
insert two projects with them and ask the database to return their ids. To
avoid repeating `"field" =: value` in every row, we use the function `named`: it
takes the list of field names once and attaches them to the values of each row
in order.

```haskell
--8<-- "app/app/Tut/FlatDml.hs:insert-projects"
```

```text
--8<-- "out/tutorial.txt:insert-projects"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:insert-projects-sql"
    ```

`named` returns exactly the same rows as `(=:)` would, they are just shorter to
write. The values are taken as they are: the constructor `Project_status_active`
or `Nothing` need no wrapping. If the number of names does not match the number
of values in a row, the program does not compile.

With the result we did the opposite: `plains @("id" := Int64)` drops the field
names and keeps only the values, so the output is just a list of ids. The type
passed to `plains` also defines the result type of `insertSch`, and thus the
columns in `returning`. We do the same in the rest of this chapter.

The columns `id` and `created_at` are not in the input data: their default
values fill them. But all mandatory columns, that is, declared `NOT NULL` and
having no default, must be in the input data, and this is checked at compile
time. If we forget `title`, for example, the program does not build:

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "projects"
  Missing mandatory fields: '["title"]
```

The same error appears when a new mandatory column is added to the table and
we regenerate `Sch.hs`: the compiler shows every place where rows are inserted
into this table.

:mag: The column `tags` is a `text[]` array. `pg-schema` has its own type
`PgArr` for arrays. Unlike `PGArray` from `postgresql-simple`, its `ToField`
instance knows the type of the elements in the database, so arrays of any type
can be inserted. The elements of `PgArr` have type `Maybe`, like the elements
of PostgreSQL arrays, and the function `pgArr'` builds an array from values
without `Nothing`.

## Update by condition

`updateByCond` writes the same values to all rows that match a condition. Let
us archive the second project and ask for the updated rows back. We will need
the type of the `project_status` enum more than once, so we define a synonym
for it:

```haskell
--8<-- "app/app/Tut/Common.hs:project-status"
```

```haskell
--8<-- "app/app/Tut/FlatDml.hs:update-by-cond"
```

```text
--8<-- "out/tutorial.txt:update-by-cond"
```

The first argument after the connection is the new values, the second is the
condition. The conditions are the same as in queries for reading and are
checked the same way; they are covered in detail in the chapter
[Select](04-select.md). If you only need the number of updated rows, use
`updateByCond_`.

`updateByCond` itself returns only the rows from `returning`. To see its SQL,
use `updateText`, which needs the same types and the same condition:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:update-text"
```

```text
--8<-- "out/tutorial.txt:update-text"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:update-text-sql"
    ```

The parameter list here holds only the value from the condition. `updateText`
does not get the new column values; `updateByCond` puts them before the
condition parameters, in place of the first `?` marks.

Column names in the new values are checked as everywhere else. A typo
`"status1"` instead of `"status"` gives a familiar error:

```
• In schema Sch
  for table "tut" ->> "projects"
  name "status1" is not defined.

  Valid values are:
    Fields: id, owner_id, title, status, tags, created_at.
    Foreign key constraints: projects_owner_id_fkey, project_log_project_id_fkey, tasks_project_id_fkey.

  Your source or target type or renamer is probably invalid.
```

## Delete

Delete works the same way, only without new values, and `deleteByCond` returns
the number of deleted rows together with the query text and its parameters:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:delete-by-cond"
```

```text
--8<-- "out/tutorial.txt:delete-by-cond"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:delete-by-cond-sql"
    ```

## Update by key

Often you need to change not all rows that match a condition but a few
specific rows, each in its own way. That is what `updateByKey` is for. It takes
a list of records, each holding the key of a row and the new values, and for
each record runs an `update` by that key. The key may be the primary key or a
unique key of the table, and `pg-schema` decides which one from the fields of
the record. In the example below it is the primary key `id`:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:update-by-key"
```

```text
--8<-- "out/tutorial.txt:update-by-key"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:update-by-key-sql"
    ```

There is no row with `id = 999`, and `updateByKey` inserts nothing. That is why
the result has the type `[Maybe r]`: for each input record either the updated
row or `Nothing` if no row with that key was found. The type `r` itself stays
plain, without `Maybe`. Importantly, the results are guaranteed to come in the
same order as the input records, and there are exactly as many of them. So
each `Nothing` is easy to match with the record whose row was not found: for
example, by `zip`ping the input list with the result. Note also the
`IS NOT DISTINCT FROM` in the condition: it makes the key match even when it
contains `NULL`, which matters for unique keys over nullable columns.

If a record contains no full key, there is nothing to update by, and the
program does not compile. The compiler lists the keys that can be used:

```
• We can't upsert data because for table NameNS "tut" "users"
  not all mandatory fields are present and no full primary or
  unique key is covered by record fields.
  Missing mandatory fields: '[]
  Possible keys: ['["id"], '["email"]]
```

## Insert or update by key

`upsertByKey` inserts a row, and if a row with the same key already exists,
updates it. In SQL this is `insert … on conflict (key) do update`. Since the
row may turn out to be new, the record must contain all mandatory columns, as
for `insertSch`, and also a full key by which the conflict is detected. Let us
update the user with `id = 3` and insert a new one with `id = 100`:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:upsert-by-pk"
```

```text
--8<-- "out/tutorial.txt:upsert-by-pk"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:upsert-by-pk-sql"
    ```

The key does not have to be the primary key. The `users` table has a unique key
on `email`, and if a record has no `id` but has an `email`, the conflict is
detected by it:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:upsert-by-uk"
```

```text
--8<-- "out/tutorial.txt:upsert-by-uk"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:upsert-by-uk-sql"
    ```

The user with the address `quentin@example.com` already existed, so only the
name changed and the `id` stayed the same. The new user "Mia" got `id = 5`, not
4: on the conflict PostgreSQL had already taken the next value of the `id`
sequence before it detected the conflict, and that value was lost. Every
`insert … on conflict` behaves this way, and it does not affect the program.

`upsertByKey` does not accept a record with only a key: such a row cannot be
inserted because mandatory columns are missing.

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "users"
  Missing mandatory fields: '["name"]
```

To update a row by its key alone, use `updateByKey`.

## Which operation to choose

If the rows are certainly new, use `insertSch`. If the rows already exist and
must be updated by key, and a missing row is no reason to create one, use
`updateByKey`. If a row may be missing and should then be created, use
`upsertByKey`, and the record must contain all mandatory columns. Finally,
`updateByCond` and `deleteByCond` work not with individual rows but with all
rows that match a condition. Which types are allowed in `returning` for each
operation is summarized in a table in the
[reference](appendix-reference.md#returning).

## Summary

- Rows are inserted with `insertSch`, and `named` saves repeating field names
  in every row.
- The result type (for example, the one passed to `plains`) defines the columns
  in `returning`.
- `updateByCond` and `deleteByCond` work with all rows that match a condition.
- `updateByKey` returns `Nothing` for keys that were not found, in the same
  order as the input.
- `upsertByKey` requires all mandatory columns and a full key.

## What next

The conditions we used in `updateByCond` and `deleteByCond` are covered in
detail in the chapter [Select](04-select.md), together with all the ways to read
data. Inserting and updating data in several related tables at once is the
subject of the chapter [Tree DML](05-tree-dml.md).
