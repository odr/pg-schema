# Compiler errors

In the previous chapters we made mistakes on purpose more than once and looked
at what the compiler said. This chapter collects those errors. You do not have
to read it in order: it is more convenient to come back to it when the compiler
gives you an unclear message and find a similar one here.

First, how to read such messages. A GHC error consists of several items, each
starting with `•`. The first item is the message of `pg-schema` itself: which
table, which field and what is wrong with it. The following items
(`In a stmt of a 'do' block`, `In the first argument of …`) show where in the
code the error happened, and usually point at the call of an operation
(`selectSch`, `insertJSON` and so on) rather than at the type definition: a row
type is checked where it meets an annotation. So one error in a type may give
several identical messages — one for each call that uses the type. You fix it
once, in the type definition, and all these messages go away. The table and
field from the first item help to find the type.

All examples below show only the first item of the message.

## Names

The most common error is a typo in a name. If a table is not found, the message
is short:

```
• In schema Sch.Sch the table NameNS "tut" "Users" is not defined.
```

If a field or a relation is not found, the compiler lists all columns and
foreign keys of the table, so the right name is usually visible at once:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "titel" is not defined.

  Valid values are:
    Fields: project_id, seq, title, priority, payload, watchers.
    Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey.

  Your source or target type or renamer is probably invalid.
```

Both the name in the message and the list of valid names are database names. A
name from your code first goes through the renamer (which turns a Haskell name
into a database name), and the message shows the result. With `titel` you do
not notice it, but if you write `"ownerID"` instead of `"ownerId"`, the renamer
`CamelToSnake` turns it into `owner_i_d`, and the compiler reports
`name "owner_i_d" is not defined`. If the message shows an unexpected name,
check what the renamer makes of what you wrote. The same message appears for
names in conditions and ordering, for record fields and for `PgTag` fields.

For a relation name in `qPath` the message shows both the database name (in
quotes) and what was written in the code (in parentheses), and splits the valid
relations by direction:

```
• In schema Sch
  for table NameNS "tut" "projects"
  relation "tasks_project_fkey" ("tasksProjectFkey" after applying the renamer) is not defined.

  Valid values are:
    Foreign key constraints to parent tables (qPathFromHere or qPath): projects_owner_id_fkey.
    Foreign key constraints from child tables (qPathToHere or qPath): project_log_project_id_fkey, tasks_project_id_fkey.
```

For a table related to itself `qPath` does not fit, and the compiler suggests
what to use instead (see the chapter [Select](04-select.md) for details):

```
• qPath cannot be used for self-reference relation.
  Use qPathFromHere or qPathToHere.

  Table: NameNS "tut" "users"
  Relation: "users_manager_id_fkey"
```

## Field types and NULL

The Haskell type of a field must match the column type. If a column allows
`NULL`, the field must be `Maybe`, and if it does not, it must not be:

```
• You can't use Maybe for mandatory fields

  Table: NameNS "tut" "users"
  DB Field name: "name"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Maybe Text
```

```
• You have to use Maybe for nullable fields

  Table: NameNS "tut" "users"
  DB Field name: "email"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Text
```

The same rule applies to values in conditions: a nullable column must be
compared with a value of type `Maybe`, for example `"managerId" =? Just 1`. The
message is the same, "You have to use Maybe for nullable fields".

If the Haskell type simply does not fit the column type, the message is less
friendly:

```
• Could not solve: ‘CanConvert1
                      Sch
                      (NameNS "tut" "users")
                      "email"
                      (NameNS "pg_catalog" "text")
                      (TypDef "S" Nothing '[])
                      Int’
```

Read it like this: for the table `tut.users` and the field `email` of type
`pg_catalog.text` there is no conversion to `Int` (`TypDef "S" …` describes the
type in the database, and `S` means a string type). So the field type must be
changed to a suitable one, here `Text`. Which Haskell types fit which PostgreSQL
types is listed in the appendix [Data types](appendix-types.md).

The error looks similar when the type of a value in a condition is not set, for
example `"priority" <? 50` without an annotation: a numeric literal can be of
any type, and the compiler does not know which one to pick. The message shows a
variable like `v0` instead of a Haskell type:

```
• Could not deduce ‘CanConvertMaybe
                      Sch
                      (NameNS "tut" "tasks")
                      "priority"
                      (NameNS "pg_catalog" "int4")
                      False
                      (TypDef "N" Nothing '[])
                      v0’
    arising from a use of ‘<?’
```

It is enough to give the type: `"priority" <? (50 :: Int32)`.

If you forgot to derive `Generic` for a record, the message is long and
obscure, with internal `pg-schema` names like `WalkLevelAnn` and `GToJSONCols`.
You can recognize it by the fragment `GHC.Internal.Generics.Rep Task`: `Rep` is
the representation of a type via `Generic`, and since it does not reduce, the
instance is missing.

## Aggregates

For aggregates the result type of the SQL function is checked. For example,
`avg` over an `integer` column returns `numeric`, and `Double` does not fit it:

```
• Aggregate 'avg' over a field of type 'int4' returns SQL type 'numeric',
  but Haskell type Double is not compatible with it.
  Use Scientific or Rational instead.

  Table: NameNS "tut" "tasks"
  DB Field name: "priority"
  DB Field type: NameNS "pg_catalog" "int4"
  Haskell Field type: Aggr AAvg (Maybe Double)
```

Recall (chapter 04) that `Aggr` is used in queries without grouping and
requires `Maybe` inside, and `Aggr'` in queries with grouping. If `Maybe` is
needed, it goes inside the aggregate (`Aggr AMax (Maybe Int32)`), not outside
(`Maybe (Aggr' AMax Int32)`):

```
• You can't use Maybe for aggregates. Use Maybe inside Aggr'

  Table: NameNS "tut" "tasks"
  DB Field name: "priority"
  DB Field type: NameNS "pg_catalog" "int4"
  Haskell Field type: Aggr' AMax Int32
```

## Mandatory columns and keys

For an insert, the input data must contain all mandatory columns, that is,
those declared `NOT NULL` and having no default value:

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "projects"
  Missing mandatory fields: '["title"]
```

The same error appears after a mandatory column is added to the table and
`Sch.hs` is regenerated: the compiler shows every place where rows are inserted
into this table. `upsertByKey` gives the same error if a record contains only a
key: the row may turn out to be new, and there is nothing to insert it with. To
update a row giving only its key, use `updateByKey`. (`upsertJSON` behaves
differently here: a row without all mandatory columns is not inserted but only
updated by key, which is why `Maybe` appears in its result.)

Updating rows by key means `updateByKey`, `updateJSON`, and also `upsertJSON`
for rows that lack some mandatory columns. All of them need a full primary or
unique key. If there is none, the compiler lists the possible keys:

```
• We can't upsert data because for table NameNS "tut" "tasks"
  not all mandatory fields are present and no full primary or
  unique key is covered by record fields.
  Missing mandatory fields: ["seq", "title"]
  Possible keys: '[["project_id", "seq"]]
```

The message is shared by several operations, so it talks about upsert and
mandatory columns even if you called `updateByKey` or `updateJSON`. For them
the last line matters: one of the listed keys must be entirely in the type. In
operations with related tables, columns that come from the parent (here
`project_id`) need not be in the type.

## Result shape of tree operations

In `upsertJSON` and `updateJSON` a row that is updated by key may not be found,
and then its place in the result holds `Nothing`. So the type of such a row in
the result must be `Maybe`. The words "update or key-only upsert" in the
message mean exactly this: `updateJSON`, or an `upsertJSON` row that has only a
key. For child rows the compiler says it like this:

```
• Returning must use Maybe here (update or key-only upsert).
  At path: '[]
  Table: NameNS "tut" "projects"
  Field: "tasksProjectIdFkey"
  Got: ("seq" := Int32) :. ("priority" := Int32)
```

For the root of `upsertJSON` without all mandatory columns, the message
explains the reason in more detail:

```
• Upsert returning row must be wrapped in Maybe:
  the input row lacks mandatory fields ["owner_id", "title"]
  so it is only updated by key and a row that is not found is returned as null.
  Table: NameNS "tut" "projects"
  Got: ("id" := Int64)
       :. ("tasksProjectIdFkey"
           := [("seq" := Int32) :. ("title" := Text)])
```

In the first case the fixed type looks like this:
`"tasksProjectIdFkey" := [Maybe ("seq" := Int32 :. "priority" := Int32)]`,
that is, `Maybe` wraps each element of the list, not the whole list. The root of
the result of `updateJSON` must be `Maybe` too. But for a row that has all
mandatory columns `Maybe` is forbidden: such a row is inserted or updated via
`insert … on conflict` and is always present in the result. All these rules are
summarized in a table in the [reference](appendix-reference.md#returning).

## Query structure

`qLimit`, `qOffset` and `qDistinct` are allowed only at the root of a query and
in a `qPath` to child rows. In a `qPath` to the parent they make no sense: a row
has one parent, and there is nothing to limit. The message about it is,
unfortunately, not the clearest:

```
• Couldn't match type ‘pg-schema-0.8.1.1:PgSchema.Schema.RelOne’
                 with ‘pg-schema-0.8.1.1:PgSchema.Schema.RelMany’
    arising from a use of ‘qLimit’
```

If you see `RelOne` and `RelMany`, check whether `qLimit`, `qOffset` or
`qDistinct` stands inside a `qPath` to the parent.

## Helper functions

`named` from chapter 03 attaches a list of field names to the values of each
row. It checks that there are as many names as values in a row. If there are
fewer names, the last name would get several values at once:

```
• named: there are fewer field names than fields in the row.
    The last name "status" is given to more than one field: PGEnum
                                                              Sch ("tut" ->> "project_status")
                                                            :. PgArr Text
```

`UnsafeCol` from chapter 04 describes a result field with an arbitrary SQL
expression. It checks that the expression has as many `?` marks as there are
columns in the list. If there are more columns, the message is this one (and if
there are more `?` marks, a similar one, worded the other way round):

```
• Count of field names in the list of fields is more than count of question marks in expression.
```

## Summary

- The first item of a message is the message of `pg-schema`; the others show
  the place in the code.
- The name in a message is the database name, that is, after the renamer.
- One error in a type gives a message for each call, but is fixed once.
- Columns that allow `NULL` need `Maybe` both in the field type and in the
  value of a condition.
- A message about keys is read by its `Possible keys` line.
- `RelOne`/`RelMany` next to `qLimit`, `qOffset` or `qDistinct` means they stand
  in a branch to the parent.

## What next

This chapter is useful as a reference for compiler messages. How to live with
`pg-schema` in a real project where the database schema changes is covered in
the chapter [In practice](07-practice.md), and the rules for result types and
the limitations are collected in the [reference](appendix-reference.md).
