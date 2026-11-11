# Select

In this chapter we look at reading data in detail. We already know that the
type defines the shape of the result of `selectSch`: which fields to read,
which related tables to pull in, and whether they are nested as a list or as a
single row. Here we see which result shapes there are, and then move on to
describing the query: conditions, following relations, ordering, limiting the
number of rows, aggregates and, finally, the way out for when `pg-schema`
cannot express what you need.

## Data for the examples

The examples of this chapter are in `tutorial/app/app/Tut/Select.hs`. Before
them the program recreates the schema and fills it with a small data set: four
users, three of whom have a manager, four projects in different statuses and
six tasks. Here is how the data is filled in:

??? note "Filling the database for the chapter's examples"

    ```haskell
    --8<-- "app/app/Tut/Select.hs:sel-seed-types"
    ```

    ```haskell
    --8<-- "app/app/Tut/Select.hs:sel-seed"
    ```

All examples are run by the same helper function `run`. It prints the result
rows one per line, then the query parameters and, separately, the formatted
SQL:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-run"
```

## Result shape

Let us start with a query without any conditions. We read projects together
with their owners and tasks:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-tree-types"
```

```haskell
--8<-- "app/app/Tut/Select.hs:sel-tree"
```

```text
--8<-- "out/tutorial.txt:sel-tree"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-tree-sql"
    ```

Two fields of the type `Project` describe relations. `tasksProjectIdFkey` is,
as in the chapter [Quick start](01-wow.md), the foreign key from `tasks` to `projects`: for a project it
leads to child rows, so the field is a list. `projectOwner` goes the other way:
the key `projects_owner_id_fkey` points from `projects` to `users`, so a
project has exactly one owner, and the field is a single row of type `Owner`.
We made up the name `projectOwner` ourselves. The renamer `MyRenamer` from
the chapter [Core workflow](02-core-workflow.md) translates it into the real name of the foreign key:

```haskell
--8<-- "app/app/Tut/Common.hs:renamer"
```

Note that `Owner` is not a record but a `PgTag`: `type Owner = "name" := Text`.
The two notations can be mixed freely. A field of a Generic record can be a
`PgTag` row, a list of them or a `Maybe`, and a record, in turn, can be part of
a `:.` chain, as in the chapter [Core workflow](02-core-workflow.md). For a small row of one or two fields you do not
have to define a separate record.

The SQL shows how `pg-schema` builds such a query. The parent row is attached
with a `join`, and child rows are gathered by a subquery into a JSON array, so
the whole result is read by one query.

If a foreign key allows `NULL`, there may be no parent. Then the field must
have a `Maybe` type, and a `left outer join` is used instead of a `join`. A good
example is the column `users.manager_id`: it refers to the same table `users`.
The same foreign key can be followed both ways: to the manager (one row or
`Nothing`) and to the subordinates (a list). `pg-schema` decides the direction
from the field type, and the renamer lets us give both fields clear names,
`manager` and `subordinates`:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-self-types"
```

```haskell
--8<-- "app/app/Tut/Select.hs:sel-self"
```

```text
--8<-- "out/tutorial.txt:sel-self"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-self-sql"
    ```

## Conditions

Conditions are set with the function `qWhere` inside `qRoot`. For comparing
with a parameter there are the operators `=?`, `<?`, `>?`, `<=?` and `>=?`, and
for matching strings against a pattern `~=?` (`like`) and `~~?` (`ilike`).
Conditions are combined with the operators `&&&` and `|||`, and `pnot` negates
them. `&&&` binds tighter than `|||`, like `and` and `or` in SQL, so no extra
parentheses are needed:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-cond"
```

```text
--8<-- "out/tutorial.txt:sel-cond"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-cond-sql"
    ```

Here we selected active projects whose title contains "site" and, besides
them, inactive projects with `id` 3 or 4. `pinArr` checks that a value is in a
list. The list may be empty, and then the condition is simply false. Note that
the column `id` is used in the condition, although it is not in the result:
conditions may refer to any column of the table.

Columns that allow `NULL` deserve special attention. To check that a value is
missing, use `pnull`. To compare with a value that may itself be `Nothing`,
there is the operator `=??`: it turns into `is not distinct from`, and
`Nothing` matches `NULL` with it. Another way to check membership in a list is
`pin`, which takes a non-empty list (`NonEmpty`):

```haskell
--8<-- "app/app/Tut/Select.hs:sel-null"
```

```text
--8<-- "out/tutorial.txt:sel-null"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-null-sql"
    ```

The types of values in conditions are checked as strictly as the result fields.
If a column allows `NULL`, the value it is compared with must be `Maybe` too.
For example, `"managerId" =? (1 :: Int64)` does not compile:

```
• You have to use Maybe for nullable fields

  Table: NameNS "tut" "users"
  DB Field name: "manager_id"
  DB Field type: NameNS "pg_catalog" "int8"
  Haskell Field type: Int64
```

The right way is `"managerId" =? Just (1 :: Int64)`. Note that the message says
`manager_id`: names in conditions, like result fields, go through the renamer,
so you can write them either in camelCase or as they are named in the database.

## Result branches: qPath

Conditions, ordering and row limits can be set not only for the root table but
for any branch of the result. That is what `qPath` with a relation name is for:
inside it everything refers to the table at the other end of the relation. In
the next example we take only active projects, order them by title in reverse,
and keep one task for each project — the one with the highest priority among
those with a priority below 50 (`ascf` and `descf` order by a field ascending
and descending):

```haskell
--8<-- "app/app/Tut/Select.hs:sel-path"
```

```text
--8<-- "out/tutorial.txt:sel-path"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-path-sql"
    ```

The conditions on the branch go into the subquery that gathers the tasks, so
they affect only the contents of the list of tasks, not which projects get into
the result. A project without matching tasks stays in the result with an empty
list. That is how child branches behave. With a parent branch it is different:
the parent is attached with a `join`, so conditions set in a `qPath` to it (for
example, `qPath "projectOwner"`) go into the `where` of the main query and
filter the root rows.

!!! warning "Branch conditions are tied to a result field"
    As we saw in the chapter [Quick start](01-wow.md), conditions, ordering and limits from `qPath` apply
    to the result field with the same relation name. If the type has no such
    field, they silently do not get into the query. For a parent branch this
    means that the filter on root rows disappears together with the field.

## Filtering rows by related tables: pchild and pparent

Sometimes you need the opposite: to select the rows themselves by what is in a
related table. For example, find the projects that have a task with a priority
below 20. That is what the condition `pchild` is for. It takes the name of the
relation together with the database schema, parameters for selecting child rows
and a condition on them. The relation name goes through the renamer as
everywhere, so we write it the same way as in `qPath`: `"tasksProjectIdFkey"`.

```haskell
--8<-- "app/app/Tut/Select.hs:sel-tut"
```

```haskell
--8<-- "app/app/Tut/Select.hs:sel-pchild"
```

```text
--8<-- "out/tutorial.txt:sel-pchild"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-pchild-sql"
    ```

The relation name here is a type: `pchild`, like `selectSch` with its
annotation, takes it as a visible type argument. You cannot write it right in
the call as `"tut" ->> "tasksProjectIdFkey"`: the operator `->>` also exists at
the value level (it builds names in ordinary code), and GHC reads such an
expression as a value. The type synonym `Tut` removes this ambiguity. `pchild`
turns into `exists` with a subquery. The selection parameters `defTabParam`
mean "all child rows". They can be narrowed: `TabParam` holds ordering
(`order`) and `limit`/`offset` (`lo`, a value of type `LO { limit, offset }`;
`lo1` is `limit 1`). This way you can express the condition "among the first N
children". Let us find the projects whose task with the highest priority value
has a priority below 50:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-pchild-first"
```

```text
--8<-- "out/tutorial.txt:sel-pchild-first"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-pchild-first-sql"
    ```

For "Website" the first task in this order is "Polish" with priority 100, for
"Docs" it is "Outline" with priority 50, so only "API" is left in the result.

`pparent` works the other way: it filters rows by their parent. Let us find the
tasks of active projects with a priority of at least 30:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-pparent"
```

```text
--8<-- "out/tutorial.txt:sel-pparent"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-pparent-sql"
    ```

The difference between `pchild`/`pparent` and `qPath` matters. `pchild` and
`pparent` decide which rows get into the result, but do not add the related
rows themselves to the result. A `qPath` to a child branch, on the contrary,
does not affect which root rows are selected but defines what ends up in the
nested list. With a branch to a parent, as we saw above, it is different: the
parent is attached with a `join`, and conditions in a `qPath` to it filter the
root rows just like `pparent`. It is convenient to decide first which rows you
need at the root, and then tune the contents of the child branches separately.

## Ordering, limit, offset and distinct

We have already met `qOrderBy`: it takes a list of fields, each with an order
direction (`ascf` ascending, `descf` descending). `qLimit` and `qOffset` limit
the number of rows. Let us take the second and third tasks by descending
priority:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-limit"
```

```text
--8<-- "out/tutorial.txt:sel-limit"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-limit-sql"
    ```

`qDistinct` removes duplicate rows. For example, this is how to find out which
project statuses occur in the database:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-distinct"
```

```text
--8<-- "out/tutorial.txt:sel-distinct"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-distinct-sql"
    ```

The statuses are ordered as the values are declared in the enum
(`'draft', 'active', 'archived'` in `tutorial/sql/00-create-schema.sql`), not
alphabetically. `qDistinctOn` keeps one row for each value of the given fields,
and the ordering decides which row is kept. Let us keep, for each project, one
task with the lowest priority:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-distinct-on"
```

```text
--8<-- "out/tutorial.txt:sel-distinct-on"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-distinct-on-sql"
    ```

PostgreSQL requires the fields of `distinct on` to come first in `order by`,
and `qDistinctOn` adds them there itself.

Since the parent is attached with a `join`, its fields can be used in
`distinct on` and `order by` too, mixed with the fields of the root. Each field
is given inside the `qPath` of the table it belongs to, and the order of fields
in the query follows the order of the calls. Let us keep, for each combination
of task number and status of its project, one task — the one with the highest
priority:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-distinct-on-parent"
```

```text
--8<-- "out/tutorial.txt:sel-distinct-on-parent"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-distinct-on-parent-sql"
    ```

Note that for a task the field `tasksProjectIdFkey` is the single row of its
project, while for a project the field with the same name is the list of its
tasks. The relation name is the same, but the meaning depends on the table we
start from.

The query got `distinct on (t0.seq, t1.status)` and
`order by t0.seq, t1.status, t0.priority desc`: a field of the root, a field of
the parent and again a field of the root. Ordering without `distinct on` works
the same way, by calling `qOrderBy` at the root and inside a `qPath` to the
parent.

All these functions work in child branches, as we saw with `qLimit` in the
section about `qPath`. But `qLimit`, `qOffset` and `qDistinct` make no sense in
a branch to a parent, since a row has only one parent, and the compiler does
not accept them there.

## A table related to itself

For a foreign key that refers to its own table, `qPath` does not fit. `qPath`
decides the direction from the relation: whether it leads from the current
table or into it. For the relation `users_manager_id_fkey` both are true, and
the name alone does not tell whether we go to the manager or to the
subordinates. (The result type has no such ambiguity: there the direction is
seen from the field type — a list or a single row.) The compiler says so:

```
• qPath cannot be used for self-reference relation.
  Use qPathFromHere or qPathToHere.

  Table: NameNS "tut" "users"
  Relation: "users_manager_id_fkey"
```

Instead of `qPath`, use `qPathFromHere` here (by the key of the current row,
that is, to the manager) and `qPathToHere` (to the rows that refer to the
current one, that is, to the subordinates). Let us select users without a
manager or with the manager `id = 1`, and keep in the list of subordinates only
those whose name contains the letter "o":

```haskell
--8<-- "app/app/Tut/Select.hs:sel-self-path"
```

```text
--8<-- "out/tutorial.txt:sel-self-path"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-self-path-sql"
    ```

Bob's subordinate Dave is not in the list: his name has no letter "o".

The names `manager` and `subordinates` here are just two names of the same
foreign key, and the direction is set not by the name but by the function. If we
wrote `qPathFromHere "manager"` with the same condition, it would apply to the
manager and, as with any parent branch, filter the users themselves: only those
whose manager's name contains "o" would remain. On our data there are none, and
the result would be empty.

## Aggregates

Aggregate functions are also set by the result type. A field of type
`Aggr' ACount Int64` becomes `count(*)`, and `Aggr' AMax Int32` becomes `max`
over the column named like the field. The other fields without aggregates
become grouping keys, and `group by` is added automatically:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-aggr"
```

```text
--8<-- "out/tutorial.txt:sel-aggr"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-aggr-sql"
    ```

`Aggr'` is meant for queries with grouping: each group has at least one row, so
`max` over a mandatory column cannot return `NULL`, and `Maybe` is not needed.
Without grouping, however, `max` over an empty table returns `NULL`. For such
queries there is `Aggr`: it requires `Maybe` for all functions except `count`.
The compiler does not forbid `Aggr'` without grouping, but then a query to an
empty table fails when reading the result, so without grouping it is better to
always use `Aggr`:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-aggr-total"
```

```text
--8<-- "out/tutorial.txt:sel-aggr-total"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-aggr-total-sql"
    ```

The functions `count`, `min`, `max`, `sum` and `avg` are supported.

## jsonb and arrays

A `jsonb` column can be read into any type with the `FromJSON` and `ToJSON`
instances from `aeson`. Where `postgresql-simple` itself parses or passes
values, you also need `FromField` (reading at the root of a query) and
`ToField` (single-table writes, such as the `insertSch` that fills this
chapter's data). They are easiest to define with `fromJSONField` and
`toJSONField`. The column `tasks.payload` holds the estimate of a task:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-payload"
```

Arrays are read into the type `PgArr`, which we met in the chapter [Single-table DML](03-flat-dml.md). Its elements
have type `Maybe`, because a PostgreSQL array may contain `NULL`, so in the
output the watchers look like `[Just 2, Just 3]`:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-json-arr"
```

```text
--8<-- "out/tutorial.txt:sel-json-arr"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-json-arr-sql"
    ```

## The way out: UnsafeCol, pUnsafeCond and UnsafeOrd

A lot can be written with `pg-schema`, but not everything. It is a pity when a
big query over twenty tables does not fit the scheme because one condition has
to compare the sum of two fields. For such cases there are "unsafe" tools: they
let you put a piece of SQL into a query, while the compiler still checks
everything else. `UnsafeCol` describes a result field as an arbitrary SQL
expression: the type lists the columns substituted for the `?` marks, the
expression itself and the type of the result. The `?` marks here stand for
columns, not query parameters. The column names are checked, but the expression
and whether its result matches the type (`Int32`) are not: an error in them
shows up only at run time. `pUnsafeCond` sets a condition as arbitrary text,
and `UnsafeOrd` an ordering expression together with its direction. Both build
text in a small monad `CondMonad`, where `qual @"field"` returns the name of a
column of the current table with the right alias. `qual` does not check the
field name, so a typo in it also shows up only at run time:

```haskell
--8<-- "app/app/Tut/Select.hs:sel-unsafe"
```

```text
--8<-- "out/tutorial.txt:sel-unsafe"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:sel-unsafe-sql"
    ```

Here we selected the tasks whose number plus priority is greater than 40,
computed the length of the title for each, and ordered the tasks by that
length. The programmer, not the compiler, is responsible for such expressions,
so use them only when the regular tools really are not enough. A value from the
program, like the number 40 in our condition, is better passed as a parameter
than pasted into the text: `tell` adds it to the parameter list, and a `?`
stands in its place in the text. This keeps SQL injection out of the query.

## Summary

- The result type defines child lists (`[T]`), a parent (`T`) and an optional
  parent (`Maybe T`).
- Conditions are built from the operators `=?`, `~~?`, `&&&`, `|||`, `pnot`,
  `pin`, `pinArr`, `pnull`, `=??`.
- `pchild` and `pparent` filter root rows, a `qPath` to a child branch defines
  its contents, and a `qPath` to a parent filters root rows too.
- Ordering can be set at any level, and `limit`, `offset` and `distinct` at the
  root and in child branches.
- For a table related to itself, use `qPathFromHere` and `qPathToHere`.
- Aggregates are set by the types `Aggr` and `Aggr'`.

## What next

We can now read data in any shape we need. Writing data into several related
tables at once — projects together with tasks and events — is also done in one
operation, and that is the subject of the chapter [Tree DML](05-tree-dml.md).
The full list of rules for result types is in the
[reference](appendix-reference.md).
