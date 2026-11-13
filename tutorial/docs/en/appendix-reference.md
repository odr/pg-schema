# Reference

This appendix collects the rules that the chapters show by example. It is handy
to keep at hand when you need to quickly recall what an operation allows.

All the rules below are checked at compile time: if a condition does not hold,
the program does not build, and the compiler tells what exactly is wrong.

How PostgreSQL types map to Haskell types, and how to add your own types, is
described in the appendix [Data types](appendix-types.md).

## Key-based operations

For one table there are three operations that work with rows individually. They
differ in what the input record must contain and in which SQL is run.

| Operation | What the input record must contain | SQL |
|-----------|------------------------------------|-----|
| `insertSch` | all mandatory columns | `insert` |
| `updateByKey` | a full primary key or a full unique key | `update` by key, no insert |
| `upsertByKey` | all mandatory columns **and** a full primary or unique key | `insert … on conflict (key) do update` |

Mandatory columns are those declared `NOT NULL` without a default value. If the
record has the primary key, it is used; otherwise the first unique key all of
whose columns are in the record (unique keys over `NOT NULL` columns are tried
first). `upsertByKey` does not accept a record without the full set of
mandatory columns: to change a row by key without them, use `updateByKey`. If
all fields of an `upsertByKey` record belong to the key, an existing row is
left as is and returned unchanged.

## Returning

The result type defines which columns go into `returning`. Different operations
have different requirements for it.

| Operation | Rule for the result type |
|-----------|--------------------------|
| `insertSch`, `upsertByKey` | a plain row with any columns of the table; `Maybe` on the whole row is forbidden |
| `updateByKey` | a plain row with any columns of the table; the result has type `[Maybe r]` and comes in the same order as the input (one element per record); `Nothing` means there is no row with that key |
| `insertJSON` | plain rows on all levels of the tree; `Maybe` on rows is forbidden |
| `upsertJSON` | if the input row has all mandatory columns, the row is without `Maybe`; otherwise the row is only updated by key and must be `Maybe`, on any level, including the root; for a child row with all mandatory columns `Maybe` is forbidden |
| `updateJSON` | both the root and the elements of child lists must be `Maybe` |

The rules about `Maybe` on the elements of child lists apply only to operations
on related tables (`*JSON`), not to single-table operations.

If in `upsertJSON` or `updateJSON` a row is not found by its key, its place in
the result holds `Nothing`, and the length of the list equals the number of
input roots.

## Limitations

Part of the data `pg-schema` passes via JSON: nested branches of a `selectSch`
result are built in the database as JSON, and the operations `insertJSON`,
`upsertJSON` and `updateJSON` pass and receive the whole tree as `jsonb`. So
the field types in these places must be convertible to JSON and back. This
gives the main limitation: a `bytea` column (type `Binary ByteString`) can be
read and written only at the root of `selectSch` and in single-table
operations, but not in nested branches and not in `*JSON`.

A custom type in a `json`/`jsonb` column needs `FromJSON` and `ToJSON`, and for
reading at the root of `selectSch` and for single-table operations also
`FromField` and `ToField` (the easiest way to define them is via
`fromJSONField` and `toJSONField`).

Other limitations met in the chapters:

- the aggregate functions `count`, `min`, `max`, `sum` and `avg` are supported,
  and the result type must match the type the SQL function returns;
- `qLimit`, `qOffset` and `qDistinct` apply only at the root of a query and in
  child branches, but not in a branch to the parent;
- for a relation of a table to itself, `qPathFromHere` and `qPathToHere` are
  used instead of `qPath`;
- `pin` takes a non-empty list (`NonEmpty`); for a list that may be empty
  there is `pinArr`;
- the nesting depth of a tree is limited by the number in the annotation `Ann`;
- conditions, ordering and limits from `qPath` apply to the result field with
  the same relation name and do not get into the query if there is no such
  field in the type.
