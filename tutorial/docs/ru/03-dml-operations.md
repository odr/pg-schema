# DML операции

В `pg-schema` реализованы базовые "плоские" DML-операции и операции с вложенными данными.

## "Плоские" операции

Реализованы базовые операции
- вставки (`insertSch`, `insertSch_`)
- изменения (`updateByCond`, `updateByCond_`)
- удаления (`deleteByCond`)

### Insert

Мы уже видели примеры использования `insertSch_` и `insertSch` в предыдущей главе.
Проверки на уровне типов гарантируют, что входные данные содержат все обязательные поля (т.е. все поля с ограничением `NOT NULL` и без значений по умолчанию).
Выходные данные (в `insertSch`) - любой набор полей той же таблицы.

Пример:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:insert-projects"
```

```text
--8<-- "out/tutorial.txt:insert-projects"
```

:mag: Для работы с массивами используется свой тип `PgArr`. Этот тип имеет улучшенный (по сравнению с `PGArray` из `postgresql-simple`) инстанс `ToField`, использующий информацию о типе элементов в БД. Это позволяет вставлять произвольные массивы. Кроме того, элементы `PgArr` опциональны (`Maybe`), что соответствует массиву PostgreSql. Функция `pgArr'` делает массив из `Just` элементов.

:thinking_face: Что будет, если мы забудем какое-то обязательное поле (или добавим новое поле в базу и перегенерируем описание)?

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "projects"
  Missing mandatory fields: '["title"]
```

### Update

Для изменения данных можно воспользоваться операцией `updateByCond` (или вариантом `updateByCond_`). Эти операции производят изменения всех записей, удовлетворяющих условию. Виды условий, поддерживаемых в `pg-schema` описаны в разделе про SELECT-запросы. Условия также проходят проверку типов. Здесь мы просто попробуем обновить по ключу:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:update-text"
```

```text
--8<-- "out/tutorial.txt:update-text"
```

Тут мы только получили текст апдейта и распечатали его и известный параметр. При вызове `updateByCond` новые значения будут добавлены в нужное место.

:mag: Пример ошибки:

```
• In schema Sch
  for table "tut" ->> "projects"
  name "status1" is not defined.

  Valid values are:
    Fields: id, owner_id, title, status, tags, created_at.
    Foreign key constraints: projects_owner_id_fkey, tasks_project_id_fkey.

  Your source or target type or renaimer is probably invalid.
```

### Delete

Операция удаления делается аналогично:

```haskell
--8<-- "app/app/Tut/FlatDml.hs:delete-by-cond"
```

```text
--8<-- "out/tutorial.txt:delete-by-cond"
```

### Upsert / update по ключу (flat, `ToRow`)

Для одной таблицы без JSON доступны три отдельные операции (с 0.8.0.0):

| API | Вход (compile-time) | Returning | SQL |
|-----|---------------------|-----------|-----|
| `insertSch` | все mandatory | bare `r'`, `[r']` | `INSERT` |
| `updateByKey` | полный PK или подходящий UK | bare `r'`, `[Maybe r']` | `UPDATE` (без insert) |
| `upsertByKey` | mandatory **и** полный PK/UK | bare `r'`, любые plain-колонки таблицы | `INSERT … ON CONFLICT …` |

`upsertByKey` **не** принимает «только ключ» или «только mandatory»: для patch по ключу без полного набора обязательных полей используйте `updateByKey`; для чистой вставки — `insertSch`.

Пример ошибки при неполных mandatory (как у `insertSch`):

```
• We can't insert data because not all mandatory fields are present.
  Table: ...
  Missing mandatory fields: ...
```

При отсутствии полного ключа в записи для `upsertByKey` / `updateByKey`:

```
• We can't upsert data because for table ...
  not all mandatory fields are present and no full primary or unique key ...
```

(для `updateByKey` срабатывает проверка `CheckHasKey` с тем же списком возможных ключей.)

## Операции со связанными таблицами

Pg-schema позволяет проводить операции сразу с несколькими связанными таблицами. Это набор операций: `insertJSON`, `insertJSON_`, `upsertJSON`, `upsertJSON_`, `updateJSON`, `updateJSON_`. Для плоских строк (`ToRow`) доступны `upsertByKey` / `updateByKey`. Все варианты выполняются одним round-trip в БД (внутри — JSON и временная функция/процедура). Корректность полей и связей проверяется во время компиляции.

### Returning: `t` и `Maybe t` (с 0.8.0.0)

| Операция | Правило для типа `r'` в returning |
|----------|-----------------------------------|
| `insert*` | везде «голые» поля, `Maybe` **запрещён** |
| `insertSch` / `upsertByKey` | bare `r'`, любые plain-колонки таблицы (`PlainOut`); обёртка `Maybe` на всей строке **запрещена** |
| `upsertJSON` | если на узле входа есть все mandatory — returning без `Maybe`; иначе на каждой list-ветке элемент **обязан** быть `Maybe`; лишний `Maybe` при полном узле — ошибка компиляции |
| `updateJSON` | на каждой list-ветке returning **обязан** быть `Maybe`; bare `t` **запрещён** |
| `updateByKey` | bare `r'` (`PlainOut`); отсутствие строки — `Nothing` в `IO [Maybe r']`, не в типе `r'` |

Правила `ReturningMatches*` и `Maybe` на list-ветках относятся к **дереву** (`*JSON`), не к плоскому `insertSch` / `upsertByKey` / `updateByKey`.

При `upsert`/`update`, если строка по ключу не найдена, в JSON-массиве returning на этой позиции будет `null` (в Haskell — `Nothing`), длина списка совпадает с числом входных корней.

Во всех этих операциях входные и выходные данные неявно сериализуются в json-объекты. В базе данных для выполнения операции создается временная функция (для версий с подчеркиванием - процедура) для обработки этих объектов.

Операции `insertJSON` и `upsertJSON` отличаются ограничениями, накладываемыми на входные данные (**не** распространяется на плоский `upsertByKey`, см. выше):
- для `insertJSON` данные должны содержать все обязательные поля, для которых нет дефолтных значений и которые не получаются из ссылочных ограничений. Так, при вставке данных в родительскую и дочернюю таблицу, идентификаторы родительской таблицы могут генерироваться базой данных при вставке и использоваться при вставке в дочернюю таблицу.
- для `upsertJSON` на каждом узле достаточно всех mandatory **или** полного первичного ключа / подходящего UK (сначала UK с колонками `NOT NULL`, при их отсутствии — UK с nullable-колонками; части ключа могут приходить из ссылочного ограничения).

Выходные структуры должны быть произвольными поддеревьями входных структур.

При исполнении `insertJSON/upsertJSON` реализуется такой алгоритм :

- На каждом уровне дерева код анализирует входные поля текущей таблицы и вычисляет, какую операцию выполнить: `UPDATE`, `UPSERT` или `INSERT`.
1. Если делается insertJSON, то переходим к п.4
2. Если есть информация о ключе (PK или первый подходящий UK с полным набором колонок в узле) и видно, что обязательные поля неполные, выбирается `UPDATE` по этому ключу.
3. Если есть все обязательные поля и полный тот же ключ (с учётом полей из parent-связей), то выбирается `UPSERT` (т.е. попытка `insert` с `on conflict update` по выбранному ключу).
4. Во всех остальных случаях выполняется обычный `INSERT`.
- После этого (в той же функции) обрабатываются вложенные массивы детей, и алгоритм рекурсивно повторяется для дочерних таблиц.

Ниже — фрагменты из `tutorial/app/app/Tut/TreeDml.hs`: общее дерево `treeIn` для вставки и `treeUps` для апсерта по существующему `id` проекта.

### insertJSON_

Выполняет вставку дерева без `returning`; в БД создаётся временная **процедура** `pg_temp.__ins`. Возвращается только сгенерированный текст (для отладки или логирования).

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-in"
```

```haskell
--8<-- "app/app/Tut/TreeDml.hs:insert-json-nr"
```

<details>
<summary>Вывод консоли (<code>make tut-from-scratch</code> / <code>make tut-run</code>)</summary>

```text
--8<-- "out/tutorial.txt:insert-json-nr"
```

</details>

### insertJSON

Тот же сценарий вставки, но создаётся **функция** с `returns jsonb`, а в Haskell возвращается результат в виде дерева тегов (по запрошенной форме `returning`).

```haskell
--8<-- "app/app/Tut/TreeDml.hs:insert-json"
```

<details>
<summary>Вывод консоли</summary>

```text
--8<-- "out/tutorial.txt:insert-json"
```

</details>

### upsertJSON_

Обновление по ключу родителя (`id` проекта) и вложенных дочерних строк; без `returning` — снова только текст процедуры.

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-ups"
```

```haskell
--8<-- "app/app/Tut/TreeDml.hs:upsert-json-nr"
```

<details>
<summary>Вывод консоли</summary>

```text
--8<-- "out/tutorial.txt:upsert-json-nr"
```

</details>

### upsertJSON

Вариант с `returning` в виде функции и разбором результата в Haskell.

```haskell
--8<-- "app/app/Tut/TreeDml.hs:upsert-json"
```

<details>
<summary>Вывод консоли</summary>

```text
--8<-- "out/tutorial.txt:upsert-json"
```

</details>

### Сравнение на одном patch (`treeCompare`)

В `tutorial/app/app/Tut/TreeDml.hs` функция `treeCompare` прогоняет один и тот же сценарий:

| Шаг | API | Ожидаемое поведение |
|-----|-----|---------------------|
| 1 | `insertJSON` `treeFull` | создаёт проект и вложенные строки |
| 2 | `insertJSON` снова | ошибка PG (дубликат) |
| 3 | `upsertJSON` `treePatch` | обновляет task; returning `Just` |
| 4 | `updateJSON` тот же patch | снова обновление; `Just` |
| 5 | `upsertJSON` `treePatchBad` (`id = 99999`) | **не** вставляет проект; `[Nothing]` |
| 6 | `updateJSON` bad id | `[Nothing]`, без новых строк |

Тип returning для patch по ключу (без всех mandatory на корне):

```haskell
type UpsOut =
  Maybe ("id" := Int64 :. "tasksProjectIdFkey" := [Maybe ("seq" := Int32 :. "title" := Text)])
```

```haskell
--8<-- "app/app/Tut/TreeDml.hs:tree-compare"
```

<details>
<summary>Вывод консоли</summary>

```text
--8<-- "out/tutorial.txt:tree-compare"
```

</details>
