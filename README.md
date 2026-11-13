The repository includes:

- a library [pg-schema](https://github.com/odr/pg-schema/tree/master/pg-schema#readme)
- a tutorial in English and Russian, published at
  https://odr.github.io/pg-schema/ (Russian: https://odr.github.io/pg-schema/ru/). Its sources are in [`tutorial/`](https://github.com/odr/pg-schema/tree/master/tutorial);
  to read it locally, run

  ```bash
  make tut-docs-serve
  ```

  and open http://127.0.0.1:8000. To rebuild the examples and their output from scratch
  you need PostgreSQL and GHC: see `tutorial/README.md` and `make tut-from-scratch`.
- an old tutorial in `pg-schema-tutorial`, which is outdated and no longer relevant.

## Examples in comments

Examples in the sources are written as `-- >>>` comments with the expected
output below them, in the format of the Eval plugin of the Haskell Language
Server: you can run and refresh them in the editor. To check all of them at
once (e.g. in CI), run

```bash
make examples-check PG_CONN="dbname=schema_test"
```

and `make examples-update` to write the actual output into the files (output
with errors is not written). Some examples need PostgreSQL: they connect with
`PG_CONN`. The output may depend on the GHC version, so use the version from
`cabal.project`. See `scripts/eval-examples.py` for details.
