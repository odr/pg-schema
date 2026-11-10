# Учебник по pg-schema (черновик)

Это практический tutorial по `pg-schema`: от первого запуска до разборов edge cases.

## Как читать этот курс

- Идите по главам по порядку: каждая опирается на предыдущую.
- Копируйте команды целиком из блоков `bash`.
- После каждой главы проходите checklist в конце.

## Требования

- GHC ≥ 9.10: библиотека использует видимые аргументы типов
  (`selectSch (MyAnn "users") …`). Tutorial проверяется на GHC 9.12.4
  (см. `cabal.project`).
- PostgreSQL и `psql`/`createdb` в `PATH`.
- Расширения GHC. Примеры написаны на `GHC2024`, в который уже входят
  `DataKinds` и `TypeApplications`. Дополнительно нужны:
    - `OverloadedStrings` — имена полей и связей задаются строками;
    - `BlockArguments` — для `qRoot do …` и `qPath "…" do …`;
    - `TypeFamilies` и `UndecidableInstances` — для своего ренеймера;
    - `DuplicateRecordFields` — если у нескольких record-типов есть
      поля с одинаковыми именами (например, `title` у проекта и задачи).

## Структура

1. [Зачем pg-schema](00-why-pg-schema.md) — зачем нужен подход schema-as-types.
2. [Быстрый старт](01-wow.md) — от пустой БД до вставки и чтения дерева данных.
3. [Рабочая модель pg-schema](02-core-workflow.md) — генерация схемы, первые insert и select.
4. [Изменение данных в одной таблице](03-flat-dml.md) — insert, update, upsert и delete.
5. [Select](04-select.md) — запросы с условиями, связи между таблицами.
6. [Изменение связанных данных](05-tree-dml.md) — запись дерева данных одной операцией: `insertJSON`, `upsertJSON`, `updateJSON`.

В [справочнике](appendix-reference.md) собраны правила, которые в главах показаны на примерах.

## Мини-checklist перед стартом

- [ ] Есть доступ к PostgreSQL и корректный `PG_CONN`.
- [ ] Проект собирается через `cabal build`.
- [ ] SQL-схема tutorial применена.
- [ ] Есть понимание, где лежат `tutorial/sql`, `tutorial/app`, `tutorial/docs`.
