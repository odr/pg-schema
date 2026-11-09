# Select

Цель главы: показать, как писать запросы с условиями через Generic-рекорды, включая `parent` и `child` условия.

## Что используем

- В этой главе везде используем **Generic-стиль** (обычные record-типы).
- Для условий используем `qWhere`, `pchild`, `pparent`.
- Для навигации по веткам используем `qPath`.

## Базовый пример на Generic-типах

```haskell
data TaskOut = TaskOut
  { seq :: Int32
  , title :: Text
  , priority :: Int32
  } deriving (Show, Generic)

data ProjectOut = ProjectOut
  { title :: Text
  , tasksProjectIdFkey :: [TaskOut]
  } deriving (Show, Generic)
```

```haskell
(rows :: [ProjectOut], (sql, params)) <- selectSch (MyAnn "projects") conn $ qRoot do
  qWhere $ "title" ~~? ("%Demo%" :: Text)
  qPath "tasks_project_id_fkey" do
    qWhere $ "priority" <? (50 :: Int32)
    qOrderBy [ascf "seq"]
```

Для двух record-типов с полем `title` нужно расширение `DuplicateRecordFields`,
а для `qRoot do` и `qPath "…" do` — `BlockArguments`.

## Условия по детям и родителям

`pchild` и `pparent` позволяют фильтровать по соседнему уровню relation-графа.
Связь для них задаётся полным именем FK вместе со схемой БД (`NameNSK`).
Оператор `->>` есть и на уровне значений, поэтому внутри выражения такое имя
удобно записывать через синоним типа:

```haskell
type Tut name = "tut" ->> name

activeStatus :: PGEnum Sch ("tut" ->> "project_status")
activeStatus = Project_status_active
```

Проекты, у которых есть задача с приоритетом меньше 20:

```haskell
(rows :: [ProjectOut], _) <- selectSch (MyAnn "projects") conn $ qRoot do
  qWhere $
       pchild (Tut "tasks_project_id_fkey") defTabParam ("priority" <? (20 :: Int32))
   &&& "status" =? activeStatus
```

И обратный проход через родителя — задачи активных проектов:

```haskell
(rows :: [TaskOut], _) <- selectSch (MyAnn "tasks") conn $ qRoot do
  qWhere $ pparent (Tut "tasks_project_id_fkey") ("status" =? activeStatus)
```

## JOIN vs вложенная ветка: в чем разница

- **JOIN-логика через `pparent/pchild`** влияет на отбор строк.
- **Вложенная ветка через `qPath`** задает, как формируется дочерний массив в результате.

Практическое правило: сначала определите, что должно попасть в root-результат, потом отдельно уточняйте shape детей в `qPath`.

## Что проверяется compile-time

- валидность `qPath` relation-имени;
- корректность полей в `qWhere`;
- совместимость `ProjectOut`/`TaskOut` с root-path структурой.

## Мини-checklist после главы

- [ ] Есть рабочий `selectSch` на Generic-рекордах.
- [ ] Понятна разница между `pchild/pparent` и `qPath`.
- [ ] Понятно, как conditions на дочерних ветках влияют на root.

## Что дальше

- Что уже умеем: писать запросы и conditions в Generic-стиле.
- Дальше: вставка и изменение связанных данных одним запросом (`insertJSON`, `upsertJSON`) — во второй части главы [«DML операции»](03-dml-operations.md).
