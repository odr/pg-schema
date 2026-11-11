The repository includes:

- a library [pg-schema](https://github.com/odr/pg-schema/tree/master/pg-schema#readme)
- a [tutorial](https://github.com/odr/pg-schema/tree/master/tutorial) (in Russian for now;
  the English translation is planned). The chapters include code and output from the
  examples, so read them as a site built with MkDocs:

  ```bash
  make tut-docs-serve
  ```

  and open http://127.0.0.1:8000. To rebuild the examples and their output from scratch
  you need PostgreSQL and GHC: see `tutorial/README.md` and `make tut-from-scratch`.
- an old tutorial in `pg-schema-tutorial`, which is outdated and no longer relevant.
