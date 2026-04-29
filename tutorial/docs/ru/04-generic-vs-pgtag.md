# PgTag и смешанный стиль

Цель главы: показать альтернативную `PgTag`-нотацию и то, как ее смешивать с Generic-рекордами.

## Почему это важно

После Generic-стиля удобно иметь второй инструмент:
- `PgTag` дает компактную точечную запись;
- удобно для локальных проекций, частичных update/insert payload;
- можно смешивать с обычными record-типами в одном проекте.

## Базовая форма PgTag

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
