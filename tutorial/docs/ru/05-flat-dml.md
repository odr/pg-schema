# Плоский DML: insert, update, delete

Цель главы: разобрать операции без дерева (`plain`), где работаем с одной таблицей за раз.

## Что входит в plain DML

- `insertSch` / `insertSch_`
- `updateByCond` / `updateByCond_`
- `deleteByCond`

Главное ограничение: payload должен содержать только plain-поля (без relation-веток).

## Плоский insert

Идея:

```haskell
-- Generic или PgTag, без дочерних relations
insertSch (AnnTut "projects") conn rows
```

Когда использовать:
- массовая вставка в одну таблицу;
- нет необходимости вставлять детей в том же запросе.

## Update by condition

Ключевой сценарий:

```haskell
updateByCond_ (AnnTut "tasks") conn updRow $
  "project_id" =? pid &&& "seq" =? seqNo
```

Это типобезопасный `UPDATE ... WHERE ...`, где:
- структура `updRow` проверяется;
- поля в condition тоже типизированы.

## Delete by condition

```haskell
deleteByCond CamelToSnake Sch ("tut" ->> "tasks") conn $
  "priority" >? (100 :: Int32)
```

Для анализа SQL можно использовать `deleteText`.

## Generic vs PgTag в plain DML

- Generic хорошо подходит для основных DTO.
- PgTag удобен для коротких точечных операций.
- Оба стиля можно свободно смешивать.

## Мини-checklist после главы

- [ ] Умеете сделать plain `insertSch`.
- [ ] Умеете написать `updateByCond` с типизированным `where`.
- [ ] Понимаете ограничение plain DML (без relation-полей).

## Что дальше

- Что уже умеем: уверенно делать CRUD по одной таблице.
- Что пока ограничивает: не разобрана запись дерева root + children.
- Дальше: в `06-tree-dml` переходим к tree DML (`insertJSON`/`upsertJSON`).
