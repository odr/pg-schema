-- tutorial/sql/migration-example.sql
-- Пример миграции для главы 07. Не входит в учебную схему:
-- после эксперимента базу восстанавливает `make tut-db-init`.
alter table tut.tasks rename column priority to weight;
alter table tut.projects add column code text not null;
