# Changelog for pg-schema

## 0.8.0.0

- **Breaking:** compile-time returning rules: insert forbids `Maybe`; update
  requires `Maybe` on every returning slot; upsert uses bare rows when all
  mandatory columns are present on input, otherwise `Maybe` (extra `Maybe` is a
  type error).
- Flat DML: `upsertByKey` / `upsertByKey_` and `updateByKey` / `updateByKey_`
  (`upsertByKey` requires all mandatory columns and a full key — always
  `INSERT … ON CONFLICT …`; `updateByKey` is key-only and never inserts).
- Tree DML: `updateJSON` / `updateJSON_` (update-only); `InsertMode` for the
  JSON pipeline.
- Tree returning: `array_append` keeps one JSON slot per input row (`null` when
  a row was not updated on upsert/update paths).
- `AllHasKeyTree`, `CheckHasKey`, `ReturningMatches{Insert,Upsert,Update}`,
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
