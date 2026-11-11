# Data types

For every field in data types the compiler checks whether its Haskell type fits
the type of the column in the database. This appendix lists the type mappings
that `pg-schema` supports itself and shows how to add your own.

## How the check works

From the generated `Sch.hs` the compiler knows three things about each column:
the name of its PostgreSQL type (for example, `"pg_catalog" ->> "text"`),
whether it allows `NULL`, and the type description `TypDef`. A type description
has three parts: the type category, the element type (only for arrays,
otherwise `'Nothing`) and the list of values (only for enums, otherwise empty).
The category is the letter from the `typcategory` field of the system catalog
`pg_type`: `A` — array, `B` — boolean, `C` — composite, `D` — date and time,
`E` — enum, `G` — geometric, `I` — network address, `N` — numeric,
`R` — range, `S` — string, `T` — interval, `U` — user-defined (for example,
`json`, `uuid`, `bytea`), `V` — bit string.

`Maybe` is checked first. A `NULL` value cannot be read into a plain type, so
for a column that allows `NULL` the field type must be `Maybe t`, and for a
mandatory column `Maybe` is forbidden. Like everything else, this is checked at
compile time; the messages about such errors are covered in the chapter
[Compiler errors](06-compiler-errors.md). Then, for the remaining type `t`,
there must be an equation of the open type family `CanConvert1`:

```haskell
type family CanConvert1 sch (tab :: NameNSK) (fld :: Symbol) (tn :: NameNSK) (td :: TypDefK) t :: Constraint
```

Its arguments are the schema, the table, the column, the name of the column
type, the description of that type and the Haskell type. The column here is the
database name (`email`, `created_at`), that is, already after the renamer. If
there is no suitable equation, the compiler reports
`Could not solve: CanConvert1 …` and lists all these arguments. In the message
they look slightly different than in code: GHC prints `"tut" ->> "users"` as
`NameNS "tut" "users"`, and `'TypDef` without the apostrophe. When you see such
a message, check the field type against the table below and, if the type was
chosen on purpose, add an equation for it as described in the section
"Custom types".

Arrays and enums are handled by the library separately. For an array the type
of its element is checked. An array value is built with the function `pgArr'`
from a list, and `unPgArr` returns a list with `Maybe` elements, because a
PostgreSQL array may contain `NULL` (`unPgArr'` returns a list without the
empty elements). For an enum the generator creates in `Sch.hs` a type with
constructors like `Project_status_active` (see the chapter
[Core workflow](02-core-workflow.md)), and they can be used directly in input
data and in conditions.

## Supported types

If a column allows `NULL`, the type from the table is wrapped in `Maybe`.

| PostgreSQL type | Haskell type |
|-----------------|--------------|
| `boolean` | `Bool` |
| `smallint` (`int2`) | `Int16` or `Scientific` |
| `integer` (`int4`) | `Int32` or `Scientific` |
| `bigint` (`int8`) | `Int64` or `Scientific` |
| `real`, `double precision` | `Double` or `Scientific` |
| `numeric` | `Scientific`, and also `Fixed k`: whether its precision matches the column is not checked |
| `text`, `varchar`, `name` | `Text` |
| `"char"` | `PgChar` |
| `citext` | `CI Text` |
| `date` | `Day` |
| `time` | `TimeOfDay` |
| `timestamp` | `LocalTime` |
| `timestamptz` | `UTCTime` or `ZonedTime` |
| `uuid` | `UUID` |
| `bytea` | `Binary ByteString` (strict or lazy) |
| `json`, `jsonb` | any type with `FromJSON` and `ToJSON` |
| enum | `PGEnum Sch ("schema" ->> "type")`, generated in `Sch.hs` |
| array | `PgArr t`, where `t` is the element type |

Plain `Int` is not supported for integer columns: use a type of the right size
(`Int16`, `Int32`, `Int64`). `PgArr`, `PgChar` and `PGEnum` are exported by
`PgSchema.DML`. `Scientific` comes from the package `scientific`, `CI` from
`case-insensitive`, `UUID` from `uuid`. Date and time types, `uuid` and
`citext` are supported as long as the `pg-schema` package flags `time`, `uuid`
and `case-insensitive` are on; all three are on by default.

## Custom types

Since `CanConvert1` is an open family, you can add your own equations to it.
This way you can, for example, keep an e-mail address in a separate wrapper
type rather than in a bare `Text`. Two things are needed for this.

First, the type itself must be able to travel to the database and back. Which
instances are needed depends on where it is used:

| Where the type is used | Required instances |
|------------------------|--------------------|
| the root of `selectSch` | `FromField` |
| input data and conditions of any operation, as well as `insertSch`, `updateByKey` and other single-table operations | `ToField` (and `FromField` for `returning`) |
| nested branches of a `selectSch` result | `FromJSON` |
| `insertJSON`, `upsertJSON`, `updateJSON` | `ToJSON` and `FromJSON` |

The simplest way is to give the type all four instances at once. For a wrapper
over a type that already has them, they are derived with `deriving newtype`.

Second, you need an equation of `CanConvert1` that allows this type for the
right columns. It can be general, for all columns of type `text` in all tables
(`CanConvert1 sch tab fld ("pg_catalog" ->> "text") ('TypDef "S" x y) Email`),
or narrow, only for one column of one table. A narrow equation is useful
because the compiler will not let you write `Email` into some other text field
by mistake. The variables `x` and `y` in the pattern `'TypDef "S" x y` mean
"any element type" and "any values"; for a scalar type these are `'Nothing` and
the empty list. (Equations of an open family must not contradict each other,
but a general and a narrow equation for the same type can live together,
because their right-hand sides are the same.)

The equation is written in your own module, not in `Sch.hs`, which is
overwritten on generation. If it is in the same module as the type itself, it
is not an orphan, and there are no warnings. You need the extensions
`DataKinds` and `TypeFamilies` (the others in the example are needed for the
queries themselves) and the import of `PgSchema.DML`, which exports
`CanConvert1` and `TypDef'`:

```haskell
--8<-- "app/app/Tut/Types.hs:types-header"
```

```haskell
--8<-- "app/app/Tut/Types.hs:types-imports"
```

```haskell
--8<-- "app/app/Tut/Types.hs:types-email"
```

Now `Email` can be used wherever the column `users.email` appears: in input
data, in the result and in conditions. The example runs on the data of the
chapter [Select](04-select.md):

```haskell
--8<-- "app/app/Tut/Types.hs:types-email-use"
```

```text
--8<-- "out/tutorial.txt:types-email"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:types-email-sql"
    ```

But for the column `users.name`, which has the same type `text`, `Email` no
longer fits. If in this example we write `"name" := Email` instead of
`"name" := Text`, the program does not compile:

```
• Could not solve: ‘CanConvert1
                      Sch.Sch
                      (NameNS "tut" "users")
                      "name"
                      (NameNS "pg_catalog" "text")
                      (TypDef "S" Nothing '[])
                      Email’
    arising from a use of ‘selectSch’
```

## A PostgreSQL type without a ready mapping

A PostgreSQL type that is not in the table above is supported the same way. The
tutorial database has such a type: the column `audit.project_log.client_ip` has
the type `inet`. Its name and category are easiest to look up in the generated
`Sch.hs`, which has a `TTypDefSch` equation for each type used:
`TTypDefSch ( "pg_catalog" ->> "inet" ) = 'TypDef "I" 'Nothing '[ ]`. The same
applies to domains and other types declared in your database.

Here you can no longer simply derive the instances with `deriving newtype`:
`FromField` for `Text` in `postgresql-simple` accepts only text types and
rejects `inet`. So `FromField` is written by hand: the value arrives in text
form, and it is enough to decode it. The `CanConvert1` equation is general this
time, for all columns of type `inet`:

```haskell
--8<-- "app/app/Tut/Types.hs:types-inet"
```

```haskell
--8<-- "app/app/Tut/Types.hs:types-inet-use"
```

```text
--8<-- "out/tutorial.txt:types-inet"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:types-inet-sql"
    ```
