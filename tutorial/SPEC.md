# ТЗ: доработка tutorial для pg-schema

Задание для Claude Code. Рабочий каталог — корень репозитория (там лежат
`cabal.project` и `Makefile`).

## Контекст

- `pg-schema/` — библиотека: type-level описание схемы PostgreSQL и
  типобезопасный DML.
- `tutorial/` — новый учебник на MkDocs Material. Пишется сначала на русском
  (`docs/ru`), на английский (`docs/en`) переводится позже.
- `pg-schema-tutorial/` — старый учебник, устарел. Новый заменяет его.
- Окружение: GHC 9.12.4 (`cabal.project`), `default-language: GHC2024`,
  БД `tutorial` (`dbname=tutorial` прописано в `app/generator/Main.hs` и
  `app/app/Main.hs`).
- Основной цикл: `make tut-from-scratch` = `tut-db-init` → `tut-run`
  (генератор `Sch.hs` + приложение, вывод сохраняется в `tutorial/out/tutorial.txt`)
  → `tut-sync-docs-source` → `tut-docs-build`.

## Что уже сделано

Сделан механизм сниппетов (подробности в `tutorial/README.md`):

- Примеры разнесены по модулям: `app/app/Tut/{Common,Output,Basics,FlatDml,TreeDml}.hs`,
  `Main.hs` вызывает их по порядку.
- Фрагменты кода помечаются `-- --8<-- [start:имя]` / `[end:имя]`, а главы
  подключают их строкой `--8<-- "app/app/Tut/X.hs:имя"`.
- `section "имя"` (`Tut.Output`) при `TUT_SNIPPETS=1` окружает вывод примера
  маркерами. `make tut-run` пишет вывод в `out/tutorial.txt`, а главы
  подключают его строкой `--8<-- "out/tutorial.txt:имя"`.
- `mkdocs.yml`: `base_path: [docs, .]`, `dedent_subsections: true`, `watch`.
- Главы 02 и 03 переведены на сниппеты.
- В `treeCompare` у проекта явный `id = 100`, чтобы повторный `insertJSON`
  действительно давал конфликт по ключу. В код добавлены `treeUps`,
  `upsertJSON_` и `upsertJSON`: раньше они были только в тексте главы 03.

**Пока не проверено:** код не компилировался, `out/tutorial.txt` собран
вручную из старых выводов, а в секции `tree-compare` стоит заглушка.

## Принятые решения

1. Код и вывод в главах подключаются только через сниппеты, вручную ничего
   не копируется. Исключение — сообщения об ошибках компиляции: они вставлены
   в главы текстом.
2. Одна глава — один модуль `Tut/<Глава>.hs`. Секция кода и секция вывода
   называются одинаково.
3. `out/tutorial.txt` хранится в git: документация собирается без БД, а
   `git diff` показывает, как изменился сгенерированный SQL.
4. Примеры рассчитаны на свежую БД, поэтому `tut-run` зависит от `tut-db-init`.
5. Порядок глав: Select идёт раньше tree DML.
6. Шаблон главы: цель → код → вывод/SQL → «:thinking_face: что будет, если…»
   (ошибка компиляции) → резюме (список без чекбоксов) → «что дальше» (с реальными ссылками на главы).
7. Перевод делается потом и затрагивает только прозу, код и вывод общие.
8. Любой SQL в выводе примеров сопровождается отформатированным вариантом
   в сворачиваемом блоке (`sqlSection` + `??? note`, см. `tutorial/README.md`).
9. Главы пишутся повествовательным стилем: связный текст, а не набор
   коротких утверждений; без лишних подзаголовков.

## Целевая структура

| Файл `docs/ru/` | Модуль `app/app/Tut/` | Статус |
|---|---|---|
| `index.md` | — | обновить список глав |
| `00-why-pg-schema.md` | — | исправить пример |
| `01-wow.md` | `Wow.hs` (новый) | переписать с кодом |
| `02-core-workflow.md` | `Common.hs`, `Basics.hs` | готова |
| `03-flat-dml.md` (сейчас `03-dml-operations.md`) | `FlatDml.hs` | дополнить |
| `04-select.md` | `Select.hs` (новый) | написать |
| `05-tree-dml.md` (вторая половина 03 + `06-tree-dml.md`) | `TreeDml.hs` | собрать |
| `06-compiler-errors.md` | — | новая |
| `07-practice.md` | — | новая |
| `appendix-reference.md` | — | новая |

## Задачи (по приоритету)

### T0. Проверить сборку и прибраться
- Если в корне лежат `snippets-makefile-mkdocs.patch` и `fix-docs-venv.patch`,
  применить их (`git apply`) и удалить.
- `make tut-from-scratch` должен проходить. Исправить ошибки компиляции и
  предупреждения `-Wall` в `Tut/*.hs`. Самое вероятное место ошибки — тип
  returning у `upsertJSON` в `TreeDml.hs`.
- Закоммитить настоящий `out/tutorial.txt`.
- Удалить ставшие ненужными файлы:
  `docs/source/snippets/hs/{MyAnn,MyRenamer,generator-main}.md`.
- Удалить цель `tut-sync-docs-source` и каталоги `docs/source/{app,sql}`:
  их не подключает ни одна глава. Если понадобится включить файл целиком,
  это делается строкой `--8<-- "sql/00-create-schema.sql"`.

### T1. Расширить схему БД (до новых глав)
- `sql/00-create-schema.sql`: добавить
  `users.manager_id bigint references users(id)` — nullable self-FK. Он нужен,
  чтобы показать родителя типа `Maybe` и `qPathFromHere`/`qPathToHere`.
- Перегенерировать `app/app/Sch.hs` и обновить сниппеты из `Sch.hs`
  (`CSchema.md`, `CTabDef.md`, `CDBFieldInfo.md`), а также тексты ошибок
  в главе 02 со списком полей таблицы `users`.

### T2. Точечные исправления
- `00`: `ProjectStatus_active` → `Project_status_active`;
  `"tasks_project_fk"` → `"tasks_project_id_fkey"`; результат `selectSch` —
  это пара `(rows, (sql, params))`.
- `01`: команды должны соответствовать Makefile/cabal (`make tut-db-init`,
  `make tut-run` или `cabal run tutorial:generator` / `tutorial:tutorial`);
  путь `tutorial/app/generator/Main.hs`; одно имя БД. Сделать `PG_CONN`
  рабочим: в генераторе `updateSchemaFile … (Left "PG_CONN")`, в приложении
  строку соединения читать из той же переменной с fallback на `dbname=tutorial`.
  Убрать служебный блок «Definition of Done» и ссылку на `02-setup`.
- `index.md`: список глав должен совпадать с `nav`. Добавить требования:
  GHC ≥ 9.10, PostgreSQL, нужные расширения GHC.
- `04`: заголовок «Select» вместо «DML операции»; `pchild`/`pparent`
  принимают `NameNSK` (`"tut" ->> "tasks_project_id_fkey"`, как `TS` в
  `test-pgs/Utils.hs`) — проверить компиляцией; определить `activeStatus`.

### T3. Глава 01 «за 15 минут» (`Tut/Wow.hs`)
Воспроизводимый путь с нуля: схема → `Sch.hs` → `insertJSON` дерева →
`selectSch` того же дерева с `qPath` → `selectText` → одна показательная
ошибка компиляции.

### T4. Глава 03 «Плоские DML» (`Tut/FlatDml.hs`)
- Выполнить `updateByCond` по-настоящему, а не только показать `updateText_`.
- Добавить `updateByKey` (с результатом `Nothing` для несуществующего ключа)
  и `upsertByKey` (по PK и по UK `users.email`).
- Таблицы правил returning перенести в приложение, а в главе показать их
  примерами.

### T5. Глава 04 «Select» (`Tut/Select.hs`) — главный пробел
Темы, по одному примеру с выводом SQL на каждую:
- record-деревья: дети `[T]`, родитель `T` / `Maybe T` (`manager_id`),
  переименование связи через ренеймер (`projectOwner`);
- `qRoot`, `qPath`, `qWhere` с операторами `=?`, `=??`, `<?`, `~~?`, `pin`,
  `pinArr`, `pnull`, `pnot`, `|||`, `&&&`;
- `pchild` с `TabParam` («среди первых N детей») и `pparent`; разница между
  отбором строк (`pchild`/`pparent`) и формой веток (`qPath`);
- `qOrderBy`, `qLimit`, `qOffset`, `qDistinct`, `qDistinctOn` на любом уровне
  дерева;
- self-reference: `qPathFromHere` / `qPathToHere`;
- агрегаты: `Aggr'`, автоматический `group by`;
- `UnsafeCol` / `pUnsafeCond` как запасной выход;
- jsonb (`tasks.payload`) и массивы (`tasks.watchers`).

Источники: `pg-schema/test-pgs/Tests/{Conditions,Hierarchy,Aggregates}.hs`,
раздел «Complex record datatype and selections» в `pg-schema-tutorial/README.md`.

### T6. Глава 05 «Tree DML» (`Tut/TreeDml.hs`)
- Объединить вторую половину 03 и `06-tree-dml.md`, затем удалить `06-tree-dml.md`.
- Исправить формулировку: `insertJSON` не требует полей, которые берутся
  из связи с родителем.
- Добавить раздел `updateJSON`. В `treeCompare` использовать варианты с
  returning, чтобы реально показать `Just` / `[Nothing]` из таблицы
  (тип `UpsOut` — для `updateJSON`).
- Транзакции: см. `test-pgs/Tests/InsertJSONTransaction.hs`.

### T7. Главы 06, 07 и приложение
- `06`: каталог типичных ошибок компиляции с разбором. Материал уже есть
  в главах 02 и 03.
- `07`: несколько схем, `GenNames.addRelations`, цикл «миграция →
  перегенерация → компилятор показывает, что сломалось», проверка в CI
  (`updateSchemaFile` возвращает `True`, если файл изменился).
- Приложение: правила returning/`Maybe`, ограничения (например, `ByteString`
  нельзя использовать в `*JSON`).

### T8. Обновить nav и README
`mkdocs.yml` nav — под новую структуру. В корневом `README.md` дать ссылку
на новый tutorial.

## Вопросы к автору (не решать самостоятельно)

1. Haddock `PgSchema.DML` утверждает, что в условиях и `QueryParams`
   используются имена из БД. Но по ChangeLog 0.6.0.0 ренеймер к ним
   применяется, а тест `prop_renamer_alias_dual_fields` использует
   `qPath "mid1RootFk"`. Что верно? От этого зависит глава 04.
   **Решено:** ренеймер применяется ко всем именам (поля, условия,
   сортировка, связи); haddock `PgSchema.DML` исправлен.
2. Опечатка «renaimer» в сообщениях об ошибках библиотеки: исправлять ли
   до того, как этот вывод окажется в главах?
3. Удалять ли `pg-schema-tutorial/` после выхода нового tutorial?

## Затрагиваемые файлы

| Что | Файлы |
|---|---|
| Главы | `tutorial/docs/ru/*.md`, позже `tutorial/docs/en/*.md` |
| Примеры | `tutorial/app/app/Main.hs`, `tutorial/app/app/Tut/*.hs`, `tutorial/app/tutorial.cabal` (`other-modules`) |
| Генератор | `tutorial/app/generator/Main.hs`, генерируемый `tutorial/app/app/Sch.hs` |
| Вывод | `tutorial/out/tutorial.txt` (генерируется, хранится в git) |
| Сборка | `Makefile` (цели `tut-*`), `tutorial/mkdocs.yml` |
| Сниппеты | `tutorial/docs/source/snippets/hs/*.md` |
| Схема БД | `tutorial/sql/00-create-schema.sql` |

Таблицы схемы `tut`: `users` (+ `manager_id`), `projects`, `tasks`,
`task_events`, тип `project_status`.

## Критерии готовности

- `make tut-from-scratch` проходит с нуля без ошибок и без предупреждений
  `-Wall` в `Tut/*.hs`.
- `mkdocs build --strict` проходит.
- В главах нет вручную скопированного кода или вывода примеров, кроме
  сообщений об ошибках компиляции.
- Ссылки «что дальше» ведут на существующие главы, nav совпадает с файлами.
