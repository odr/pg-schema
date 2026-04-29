# Pg-schema за 15 минут

Цель: за один проход увидеть генерацию схемы, tree-insert и tree-select.

## Шаг 0. Переменные окружения

```bash
export PG_CONN='dbname=pgschema_tutorial host=localhost user=postgres password=postgres'
```

## Шаг 1. Создать учебную схему в PostgreSQL

```bash
psql "$PG_CONN" -f tutorial/sql/00-create-schema.sql
```

Проверка:

```bash
psql "$PG_CONN" -c "\dt tut.*"
```

Ожидаем таблицы: `users`, `projects`, `tasks`, `task_events`.

## Шаг 2. Сгенерировать `Sch.hs`

Предполагается, что у вас есть генератор (например `tutorial/generator/Main.hs`) с вызовом `updateSchemaFile`.

Запуск генератора:

```bash
cabal run tutorial-gen
```

Проверка, что файл появился/обновился:

```bash
ls -la tutorial/app/app/Sch.hs
```

## Шаг 3. Выполнить wow-демо (insertJSON + selectSch)

Запуск демо-приложения:

```bash
cabal run tutorial-app
```

Ожидаем в выводе:
- вставка проекта с вложенными задачами/событиями через `insertJSON`;
- выборка дерева через `selectSch`;
- печать SQL через `selectText`.

## Мини-checklist после главы

- [ ] Схема `tut` создана.
- [ ] `Sch.hs` сгенерирован.
- [ ] Дерево данных вставилось одной операцией.
- [ ] Дерево прочиталось с `qPath`.
- [ ] SQL запроса выведен через `selectText`.

## Definition of Done
- Есть воспроизводимый путь “с нуля до результата” за <= 15 минут.
- Все команды копируются без редактирования.
- Есть минимум 1 пример tree insert и 1 пример tree select.
- Есть фрагмент с `selectText` и пояснение зачем он.
- В конце есть checklist “что понял читатель”.

## Что дальше

- Что уже умеем: быстро поднять пример и увидеть tree insert/select.
- Что пока ограничивает: среда и зависимости еще не формализованы.
- Дальше: в `02-setup` фиксируем рабочее окружение, чтобы все шаги были воспроизводимы.
