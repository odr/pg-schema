# Changelog for pg-schema

## Unreleased
- `named` (and type family `Named`): give field names once for a whole list of
  plain rows (`named ["ownerId", "title"] [1 :. "a", 2 :. "b"]`) instead of
  `"field" =: value` in every row. Values (e.g. enum constructors, `Nothing`)
  are used as is; the result has the same type as rows built with `(=:)`.
  Wrong number of names is reported at compile time.
- `rowsOf` / `plains` (and `rowOf` / `plain` for one row, `Plain`): rows from / to plain
  values with field names and types taken from the row type
  (`rowsOf @Root ["r1" :. 1 :. [10 :. True]]`). Rows are flattened as for
  `ToRow`/`FromRow`: parts joined with `(:.)` and Generic records give their
  fields in order, `fld := ()` gives no field; nested rows in field values
  (children, parents, optional parents) are plain too.
- `qPath` / `qPathFromHere` / `qPathToHere` with an unknown relation name now
  report `relation "..." is not defined` with the valid relations of the table
  (previously `qPath` reported a misleading self-reference error).
- Fix `RETURNING` checks of `insertJSON` / `upsertJSON` / `updateJSON`
  (`ReturningMatchesInsert` / `ReturningMatchesUpsert` / `ReturningMatchesUpdate`),
  which were silently skipped in most cases: e.g. `updateJSON` accepted a bare
  child list element (`"tasks" := [t]` instead of `[Maybe t]`) and then failed at
  runtime to decode `null` for a child not found by its key.
  The walk over the returning type dropped all fields after the first plain
  column, compared nested levels against the root row type, and checked
  `PgTag ann t` instead of the element row `t` (so a correct `[Maybe t]` as the
  first field was rejected). Now every child list at any depth is checked.
  Code that compiled only because of this may now fail to compile:
  - `updateJSON`: child list elements must be `Maybe`;
  - `insertJSON`: child list elements must not be `Maybe`;
  - `upsertJSON`: a child list element must be `Maybe` iff the child in the
    *input* row lacks mandatory columns (previously the *returning* columns
    were used) or upsert uses `ON CONFLICT DO NOTHING` with parent FK columns
    outside the conflict key; the root returning row must be `Maybe` if the
    input root row lacks mandatory columns.
- `NeedsFkKeyOnlyMaybe` matched the opposite case (a child with plain columns
  outside the conflict key, i.e. `ON CONFLICT … DO UPDATE`, which always
  returns the row); now it matches `ON CONFLICT DO NOTHING` as documented.
- `upsertJSON`: a child upserted with `ON CONFLICT DO NOTHING` whose key
  belongs to a row of another parent (parent FK outside the key) was returned
  as an object of `null` columns (a JSON decoding error) or, when the returning
  columns were all in the input, as if it were found under this parent; its own
  children were still processed. Now such a child is returned as `null`
  (`Nothing`) and its children are skipped, as for a row not found by `UPDATE`.

## 0.8.1.0
- upsertJSON: require `Maybe` on child returning when upsert uses
  `ON CONFLICT DO NOTHING` with parent FK columns outside the conflict key
  (`NeedsFkKeyOnlyMaybe` in `ReturningMatchesUpsert`)


## 0.8.0.1
- Build all with -Wall
- Small fixes in doc

## 0.8.0.0

- Fix SELECT path matching after renamer: promoted `PathElem'` (`peName`,
  `peDbName`, `peKind`) replaces `(Text, PathKind)`; runtime `pathStepMatches`
  supports point paths and broadcast by shared db fk name; independent filters
  on aliased sibling fields (e.g. `mid1RootFk` vs `mid1_root_fk2`).
- Internal SELECT paths: `[PathElemK]` in `PathCtx` / `TabOnDPathRen`; removed
  `EffPath` on renamer-only pairs; `TabOnDPath2` walks `peDbName`.
- Flat DML: `upsertByKey` / `upsertByKey_` and `updateByKey` / `updateByKey_`
  (`upsertByKey` requires all mandatory columns and a full key — always
  `INSERT … ON CONFLICT …`; `updateByKey` is key-only, never inserts, bare
  returning type `r'`, result `IO ([Maybe r'], Text)`).
- Tree DML: `updateJSON` / `updateJSON_` (update-only); `InsertMode` for the
  JSON pipeline.
- Tree returning: Maybe or Bare for `upsertJSON`.
- New constraints: `AllHasKeyTree`, `CheckHasKey`, `ReturningMatches{Insert,Upsert,Update}`,
  `FromJSON` / `ToJSON` for `Maybe` rows and `null` in returning arrays.
- Rename `TreeSch` to `HasSchema`

## 0.7.1.2
- Fix bug with aggregates (Min/Max/Sum/Avg)

## 0.7.1.1
- Add upsertJSONText, upsertJSONText_

## 0.7.1.0
- Condition DSL: `(=??)` on nullable columns generates SQL
  `IS NOT DISTINCT FROM` (same null-safe key equality as upsertJSON)
- Improve transaction behavior for insertJSON/upsertJSON
- Add tests for insertJSON/upsertJSON with transactions
- `upsertJSON` / `upsertJSON_`: optional `ON CONFLICT` targets include NOT NULL
  unique keys (primary key still first), then nullable unique keys when the table
  has no NOT NULL UK; `UPDATE` by key uses `IS NOT DISTINCT FROM` for key columns;
  `IdentityCandidates` (compile-time) lists every PK/UK from the schema
- `insertJSON` / `insertJSON_`: insert-only (plain `INSERT`, no `ON CONFLICT` or
  `UPDATE`; duplicate keys fail at the database)
- Rename `AllMandatoryOrHasPKTree` to `AllMandatoryOrHasKeyTree`

## 0.7.0.1
- Bug fixing (upsertJSON_ without change fields: on conflict do nothing)

## 0.7.0.0

- Bug fixing (updateText_ signature)
- Change signature for deleteByCond
- Add CondAnn & QueryParamsAnn

## 0.6.1.0

- Bug fixing (ins/upsertJSON for json db-fields)

## 0.6.0.0

- Using Renamer for QueryParams and Cond
- Improve error messaging
- Support for 'UnsafeCol'
- Check possibility of type conversions for select-list/insert/update (it was broken in 0.5.0)
- Check optional/mandatory for parent references is SELECT
- Added qPathFromHere/qPathToHere. Self-reference require explicit direction in qPathXXX

## 0.5.2.0

- Bug fixing (Problem with Self-references)

## 0.5.1.1

- Improve error messaging

## 0.5.1.0

- Bug fixing (TF stucks in some cases)

## 0.5.0.0

- First release on Hackage
