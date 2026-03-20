# pg-schema

*pg-schema* is a Haskell library that lifts a PostgreSQL schema description to the type level and gives you type-safe, compile-time-checked access to data.

The core idea is a type provider: from a live database (tables, primary and unique keys, foreign keys, column types—including arrays and enums) a schema representation is built that you then use with ordinary GHC types. You can have any number of such schemas; each may include any subset of tables.

To generate schemas you create a separate application that uses `PgSchema.Generation.updateSchemaFile` to produce `.hs` files describing each schema (sets of tables, relationships between them, and the types in use).

For reading and writing data, use the `PgSchema.DML` module. Queries are built from a typed EDSL, without hand-written SQL. You can describe data trees (nested records along relationships), including inserting an entire tree in one database round-trip (JSON is used internally), and fetching a tree with a single SELECT, with predicates, ordering, and limits at each level of the tree (`WHERE`, `ORDER BY`, `LIMIT/OFFSET`, and so on).

Inserts and reads use either ordinary Haskell records with a Generic instance or types of the form `"fld1" := Int32 :. "fld2" := Maybe Text`. Field names in Haskell records must match those in the database (with renaming supported) but you can work with any subset of the table's fields. Navigation along relationships uses foreign-key constraint names. Types and nullability are checked against the data layout.

SELECT and INSERT/UPSERT are implemented. UPDATE and DELETE apply to a single table.

When reading data, an EDSL sets conditions, ordering, and grouping. All of these operations are type-safe.

For inserts and updates, additional compile-time checks derive from database constraints—for example, inserts verify that required fields are present at every level.

## Requirements

**GHC ≥ 9.10**. Theoretically, one could target older GHC version at the cost of a slightly worse API without `RequiredTypeArguments`.
