# Core workflow

In the chapter [Quick start](01-wow.md) we went all the way from an empty
database to reading related data, taking a lot on faith. Now let us see how
`pg-schema` works. Working with it has two stages. First, the generator reads
the PostgreSQL metadata and builds a description of the database schema at the
Haskell type level — the module `Sch.hs`. Then, when you work with data, the
types of the input and the result define which SQL is run, and the compiler uses
the schema description to check that these types match the database. In this
chapter we look at both stages in detail and see which errors the compiler
catches.

## Generating the schema description

The schema description is created by a separate small program, the generator.
Here is our generator in full:

```haskell
--8<-- "app/generator/Main.hs"
```

The heart of it is the call of `updateSchemaFile`. It connects to the
database, reads the metadata and writes the description to the file if it has
changed. Which tables to describe is defined by the last argument, a value of
type `GenNames`:

--8<-- "snippets/hs/GenNames.md"

The description can include all tables of several database schemas, single
tables and, if needed, relations that do not exist in the database itself. It
is better, of course, to declare relations as foreign keys in the database, but
that is not always possible. The generated file gets the selected tables with
their columns, all foreign keys between them and all the types they use,
including enums. Several schemas, added relations and how to make the generator
part of your development process are covered in the chapter
[In practice](07-practice.md).

In practice `pg-schema` has been used with schemas of 30–50 tables. A large
database is better described by several `Sch` schemas, and one table may belong
to several of them.

## What is in Sch.hs

:mag: _You can skip this section on first reading._

The generated module defines an empty type `Sch`, which serves as the
type-level name of the schema, and instances of several classes for it. The
main one is `CSchema`: it lists all the tables and all the data types they use,
including our enum `project_status`:

--8<-- "snippets/hs/CSchema.md"

The operator `->>` joins the name of a database schema with the name of a table
or a type, both at the type level and at the value level. The context of the
class `CSchema`, as of the other classes that describe the schema, lets the
description be brought from the type level to the value level with
`Data.Singletons.demote`: that is how the library gets the names of tables and
columns to generate SQL.

The description of each table — its columns, primary key and unique keys — is
given by the type family `TTabDef`:

--8<-- "snippets/hs/CTabDef.md"

The most important instance for the checks is `CDBFieldInfo`. It describes
every name that may be used with a table:

--8<-- "snippets/hs/CDBFieldInfo.md"

Each name here is of one of several kinds. `'RFPlain` is a column of the table,
`'RFFromHere` is a foreign key from this table to another one (a reference to
the parent), `'RFToHere` is a foreign key from another table to this one (a
reference to child rows), and `'RFSelfRef` is a reference of a table to itself,
like `users.manager_id`. The last equation for each table fires when the name
is missing and produces a compile error with the list of valid names. Since all
these are closed type families, the error messages are detailed, as we will
soon see.

For each enum in the database an instance of the data family `PGEnum` is
generated. Its constructors are made of the type name and the enum values:

--8<-- "snippets/hs/Enum.md"

## The annotation

Every data operation takes an annotation `Ann` as its first argument. It is a
type-level value, and it is passed as a visible type argument:

--8<-- "snippets/hs/Ann.md"

The annotation holds four things. The renamer (`annRen`) translates the names
we use in the program into database names. The schema (`annSch`) is the
generated type `Sch`. The depth (`annDepth`) limits the nesting of relations in
operations with trees of data and mostly protects against infinite cycles in
types: 3–5 is usually enough. Finally, `annTab` is the base table of the
operation, the root for operations with trees.

A renamer is a type-level function from a name in the program to a name in the
database. Our renamer turns camelCase names into snake_case and gives a few
relations handy synonyms:

```haskell
--8<-- "app/app/Tut/Common.hs:renamer"
```

The library provides camelCase-to-snake_case conversion out of the box: it is
the type family `CamelToSnake`. It leaves names already written in snake_case
unchanged, so with this renamer you can write names either way.

The renamer, the schema and the depth are usually the same for the whole
program, so it is convenient to define a synonym that leaves only the table
name as a parameter:

```haskell
--8<-- "app/app/Tut/Common.hs:ann"
```

## First queries

Let us start simple: insert a user and read all users. We describe the data
with an ordinary record type:

```haskell
--8<-- "app/app/Tut/Common.hs:user"
```

Here and below `conn` is an ordinary `postgresql-simple` connection, and all
examples of this chapter are in `tutorial/app/app/Tut/Basics.hs`:

```haskell
--8<-- "app/app/Tut/Basics.hs:users-generic"
```

```text
--8<-- "out/tutorial.txt:users-generic"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:users-generic-sql"
    ```

We got back the same record we inserted, and besides it the query text and its
parameters (there are none here). Note that the type `User` does not contain
all the columns of the `users` table: the query writes and reads only the
fields of this type. The kind of operation is defined by the structure of the
input and output data, and a `Generic` instance is all it takes. The field
types and their optionality match the column types and whether the columns
allow `NULL`.

### :thinking_face: What if we make a mistake?

Let us see how the compiler reacts to mistakes in this example. If we mistype
the table name and write `MyAnn "Users"`, the message is short:

```
• In schema Sch.Sch the table NameNS "tut" "Users" is not defined.
```

The schema name is written as `Sch.Sch` here because the module with the
example imports `Sch` without any names (`import Sch ()`), so GHC prints the
type together with its module. If we rename the field `email` to `mail`, the
compiler lists all the columns and relations of the `users` table:

```
• In schema Sch.Sch
  for table "tut" ->> "users"
  name "mail" is not defined.

  Valid values are:
    Fields: id, name, email, manager_id, created_at.
    Foreign key constraints: users_manager_id_fkey, task_events_author, projects_owner_id_fkey.

  Your source or target type or renamer is probably invalid.
```

Whether a column is mandatory is checked too. The column `name` is declared
`NOT NULL`, so `name :: Maybe Text` does not compile:

```
• You can't use Maybe for mandatory fields

  Table: NameNS "tut" "users"
  DB Field name: "name"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Maybe Text
```

And the column `email` allows `NULL`, so without `Maybe` it does not work
either:

```
• You have to use Maybe for nullable fields

  Table: NameNS "tut" "users"
  DB Field name: "email"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Text
```

Finally, if the field type simply does not fit the column type, for example
`email :: Maybe Int`, the message is less friendly:

```
• Could not solve: ‘CanConvert1
                      Sch.Sch
                      (NameNS "tut" "users")
                      "email"
                      (NameNS "pg_catalog" "text")
                      (TypDef "S" Nothing '[])
                      Int’
    arising from a use of ‘insertSch_’
```

But it can be read too: in the schema `Sch`, the column `email` of the table
`tut.users` has the type `pg_catalog.text`, and there is no conversion to `Int`
for it. Which Haskell types fit which PostgreSQL types is listed in the
appendix [Data types](appendix-types.md).

## PgTag

Record types are convenient, but not always: defining a separate type for a
couple of fields is tedious, and the same field names in different types need
the `DuplicateRecordFields` extension. So `pg-schema` has a second notation,
`PgTag`, a field with a type-level name:

--8<-- "snippets/hs/PgTag.md"

`PgTag` is similar to the well-known type `PgTagged`, but has different
instances. For convenience there is the operator `(:=)` at the type level and
`(=:)` at the value level. Fields are joined with the operator `(:.)` from
`postgresql-simple`, and Generic records can be mixed in. Let us insert two
users and ask for their `id`, `manager_id` and the fields of the type `User`:

```haskell
--8<-- "app/app/Tut/Basics.hs:users-pgtag"
```

```text
--8<-- "out/tutorial.txt:users-pgtag"
```

??? note "Formatted SQL"

    ```sql
    --8<-- "out/tutorial.txt:users-pgtag-sql"
    ```

Here `insertSch` returns the result of the insert (`insert … returning`), and
the set of columns in `returning` is defined by the result type. Note that we
write `managerId`, not `manager_id`: field names go through the renamer. If we
write `managerId1` instead, the compiler reports that there is no such field and
names it as it is after the renamer, `manager_id1`:

```
• In schema Sch.Sch
  for table "tut" ->> "users"
  name "manager_id1" is not defined.

  Valid values are:
    Fields: id, name, email, manager_id, created_at.
    Foreign key constraints: users_manager_id_fkey, task_events_author, projects_owner_id_fkey.

  Your source or target type or renamer is probably invalid.
```

## Summary

- The generator builds the module `Sch.hs` from the database: tables, columns,
  keys, relations and enums at the type level.
- Every operation takes an annotation `Ann`: the renamer, the schema, the depth
  and the base table.
- The renamer translates names in the program into database names; error
  messages show names that are already translated.
- The SQL of an operation is defined by the types of the input and the result,
  and the compiler checks them against the schema description: names,
  mandatory columns and column types.
- Data can be described by Generic records, by `PgTag` fields, or by a mix of
  both.

## What next

In the chapter [Single-table DML](03-flat-dml.md) we look at all the ways to
insert, update and delete rows, and in the chapter [Select](04-select.md) at all
the ways to read them.
