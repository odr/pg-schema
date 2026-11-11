-- tutorial/sql/migration-example.sql
-- An example migration for chapter 07. Not part of the tutorial schema:
-- `make tut-db-init` restores the database after the experiment.
alter table tut.tasks rename column priority to weight;
alter table tut.projects add column code text not null;
