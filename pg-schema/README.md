# pg-schema

[*pg-schema*](https://hackage-content.haskell.org/package/pg-schema) is a Haskell library for working with PostgreSQL in which the compiler checks your queries. Two ideas make it different:

- **The schema description is taken from the database itself**, so it cannot silently drift from it. A generator reads a live PostgreSQL database and writes a Haskell module that describes its tables, columns, keys, relations and enums at the type level.
- **An SQL query is not written but built in accordance with Haskell types and checked against the generated schema description.** You describe which data you need — which fields, and which related rows along foreign keys — and get one SQL query that reads exactly that. Inserts and updates are built the same way, from the type of the input data.

A typo in a column name, a relation to the wrong table, a value of the wrong type, nested data of the wrong shape — all of these become compile errors. When the database changes, you regenerate the module, and the compiler shows every query that no longer matches it.

The [tutorial](https://odr.github.io/pg-schema) (English and Russian) explains everything step by step. Its complete, runnable code is in [`tutorial/`](https://github.com/odr/pg-schema/tree/master/tutorial).

## Workflow

### 1. Create the database

The schema is defined in PostgreSQL as usual, with SQL or migrations; pg-schema never creates or changes tables. A relation between tables is referred to by the **name of its foreign key constraint**. If you do not name a constraint, PostgreSQL names it `<table>_<columns>_fkey`:

```sql
create schema tut;
create type tut.project_status as enum ('draft', 'active', 'archived');

create table tut.projects (
  id      bigserial primary key,
  title   text not null,
  status  tut.project_status not null default 'draft'
);

create table tut.tasks (
  project_id  bigint not null references tut.projects(id), -- FK name: tasks_project_id_fkey
  seq         integer not null,
  title       text not null,
  priority    integer not null default 100,
  primary key (project_id, seq)
);
```

The tutorial schema with comments: [`tutorial/sql/00-create-schema.sql`](https://github.com/odr/pg-schema/blob/master/tutorial/sql/00-create-schema.sql).

### 2. Generate the schema description

Make a small executable that calls `updateSchemaFile` from `PgSchema.Generation` and run it after every change to the database structure (on CI it can check that the generated module is up to date):

```haskell
import Control.Monad (void)
import PgSchema.Generation

main :: IO ()
main = void $ updateSchemaFile False "src/Sch.hs"
  (Left "PG_CONN") -- connection string from this environment variable
  "Sch"            -- name of the generated module
  "Sch"            -- name of the schema type
  GenNames { schemas = ["tut"], tables = [], addRelations = [] }
```

You do not write or edit the generated module; it exports the schema type `Sch` and a Haskell type for every enum (here with the constructors `Project_status_draft`, `Project_status_active`, `Project_status_archived`). You can have any number of such schemas, each with any subset of tables.

Tutorial: [the generator](https://github.com/odr/pg-schema/blob/master/tutorial/app/generator/Main.hs) and [the generated module](https://github.com/odr/pg-schema/blob/master/tutorial/app/app/Sch.hs).

### 3. Optionally, define data types

Ordinary records with `deriving Generic` are enough — no `FromRow`/`ToRow`/`FromJSON` instances. Field names match column names (a renamer can map camelCase to snake_case), and a field named after a foreign key holds the related rows. A type may contain any subset of a table's columns:

```haskell
data Project = Project
  { title                 :: Text
  , status                :: PGEnum Sch ("tut" ->> "project_status")
  , tasks_project_id_fkey :: [Task] -- child rows along the foreign key
  } deriving Generic

data Task = Task { seq :: Int32, title :: Text } deriving Generic
```

Instead of records you can also write fields right in the type of an operation: `"title" := Text :. "priority" := Int32`.

### 4. Write queries with the EDSL, checked statically

All data functions are in `PgSchema.DML`. The annotation names the generated schema and the base table:

```haskell
type MyAnn tab = 'Ann RenamerId Sch 5 ("tut" ->> tab)

activeProjects :: Connection -> IO [Project]
activeProjects conn = fst <$> selectSch (MyAnn "projects") conn (qRoot do
  qWhere $ "status" =? Project_status_active
  qPath "tasks_project_id_fkey" do
    qWhere $ "priority" <? (50 :: Int32)
    qOrderBy [ascf "seq"])
```

This reads projects with their tasks in one SQL query; `insertJSON` / `upsertJSON` write such a tree in one round-trip. If you mistype a name, the program does not compile:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "priorty" is not defined.

  Valid values are:
    Fields: project_id, seq, title, priority.
    Foreign key constraints: tasks_project_id_fkey.

  Your source or target type or renamer is probably invalid.
```

Types and nullability are checked as well, and inserts check that mandatory columns are present at every level of the tree.

## What else

- `selectSch` reads trees of data along relationships with one `SELECT`, with conditions, ordering and `LIMIT`/`OFFSET` at each level.
- `insertJSON`, `upsertJSON`, `updateJSON` write a whole tree in one round-trip (JSON is used internally); `insertSch`, `upsertByKey`, `updateByKey`, `updateByCond`, `deleteByCond` work with a single table.
- Enums, `json`/`jsonb`, arrays and composite keys are supported; relations missing in the database can be added in the generator; raw SQL is still available for anything else.
- `pg-schema` works on top of `postgresql-simple`: operations take an ordinary `Connection`.

## Modules

- `PgSchema.Generation` — schema generation; used by your generator executable.
- `PgSchema.DML` — reading and writing data; import it in your application.
- `PgSchema.Import` — imported by the generated schema module.

## Requirements

**GHC ≥ 9.10**: the API uses visible type arguments (`selectSch (MyAnn "projects") …`). Theoretically, one could target older GHC versions at the cost of a slightly worse API without `RequiredTypeArguments`.
