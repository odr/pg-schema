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

--8<-- "source/snippets/hs/DML3.md"

:mag: Для работы с массивами используется свой тип `PgArr`. Этот тип имеет улучшенный (по сравнению с `PGArray` из `postgresql-simple`) инстанс `ToField`, использующий информацию о типе элементов в БД. Это позволяет вставлять произвольные массивы. Кроме того, элементы `PgArr` опциональны (`Maybe`), что соответствует массиву PostgreSql. Функция `pgArr'` делает массив из `Just` элементов.

:thinking_face: Что будет, если мы забудем какое-то обязательное поле (или добавим новое поле в базу и перегенерируем описание)?

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "projects"
  Missing mandatory fields: '["title"]
```

### Update

Для изменения данных можно воспользоваться операцией `updateByCond` (или вариантом `updateByCond_`). Эти операции производят изменения всех записей, удовлетворяющих условию. Виды условий, поддерживаемых в `pg-schema` описаны в разделе про SELECT-запросы. Здесь мы просто попробуем обновить по ключу:



Типизированные поля:

```haskell
type TaskTag =
  "seq" := Int32 :. "title" := Text :. "priority" := Int32
```

Значения:

```haskell
taskVal = "seq" =: (1 :: Int32)
      :. "title" =: ("Read docs" :: Text)
      :. "priority" =: (10 :: Int32)
```

## Тот же `selectSch`, но через PgTag

```haskell
type ProjectTag =
  "title" := Text :. "tasks_project_id_fkey" := [TaskTag]

(rows :: [ProjectTag], _) <- selectSch (AnnTut "projects") conn $ qRoot do
  qWhere $ "title" ~~? "%Demo%"
  qPath "tasks_project_id_fkey" do
    qWhere $ "priority" <? (50 :: Int32)
```

## Смешивание Generic и PgTag

Нормальный практический сценарий:
- доменные типы и output DTO держите в Generic;
- точечные payload (например частичный update/insert) пишите через `PgTag`.

Пример идеи:

```haskell
-- select в Generic
(xs :: [ProjectOut], _) <- selectSch (AnnTut "projects") conn qp

-- update payload в PgTag
let upd = "title" =: ("Renamed" :: Text)
```

## Когда что выбирать

- **Generic**: основная модель данных приложения.
- **PgTag**: компактные ad-hoc структуры, временные payload, быстрые эксперименты.
- **Mixed**: самый удобный стиль для реальных проектов.

## Мини-checklist после главы

- [ ] Понимаете синтаксис `:=` и `=:`.
- [ ] Можете написать `selectSch` в PgTag-стиле.
- [ ] Понимаете, как смешивать Generic и PgTag в одном модуле.

## Что дальше

- Что уже умеем: использовать обе нотации и выбирать подход под задачу.
- Что пока ограничивает: CRUD-операции еще не разобраны отдельно для plain-случая.
- Дальше: в `05-flat-dml` разбираем plain DML (`insertSch`, `updateByCond`, `deleteByCond`).
