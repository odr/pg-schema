# Учебник по pg-schema (черновик)

Это практический tutorial по `pg-schema`: от первого запуска до разборов edge cases.

## Как читать этот курс

- Идите по главам по порядку: каждая опирается на предыдущую.
- Копируйте команды целиком из блоков `bash`.
- После каждой главы проходите checklist в конце.

## Структура

1. `00-why-pg-schema` — зачем нужен подход schema-as-types.
2. `01-wow` — быстрый результат за 15 минут.
3. `02-core-workflow` — рабочая модель и pipeline библиотеки.
4. `03-read-with-conditions` — запросы и conditions в Generic-стиле.
5. `04-generic-vs-pgtag` — `PgTag` и смешивание двух нотаций.

## Мини-checklist перед стартом

- [ ] Есть доступ к PostgreSQL и корректный `PG_CONN`.
- [ ] Проект собирается через `cabal build`.
- [ ] SQL-схема tutorial применена.
- [ ] Есть понимание, где лежат `tutorial/sql`, `tutorial/app`, `tutorial/docs`.
