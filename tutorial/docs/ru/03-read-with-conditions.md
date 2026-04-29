# Запросы и conditions (Generic)

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
qp = qRoot do
  qWhere $ "title" ~~? "%Demo%"
  qPath "tasks_project_id_fkey" do
    qWhere $ "priority" <? (50 :: Int32)
    qOrderBy [ascf "seq"]
```

## Условия по детям и родителям

`pchild` и `pparent` позволяют фильтровать по соседнему уровню relation-графа.

Пример (идея):

```haskell
qWhere $
     pchild "tasks_project_id_fkey" defTabParam ("priority" <? (20 :: Int32))
 &&& "status" =? activeStatus
```

И обратный проход через родителя:

```haskell
qWhere $
  pparent "tasks_project_id_fkey" ("status" =? activeStatus)
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
- Что пока ограничивает: не показана альтернативная `PgTag`-нотация.
- Дальше: в `04-generic-vs-pgtag` разбираем `PgTag` и смешивание `Generic + PgTag`.
