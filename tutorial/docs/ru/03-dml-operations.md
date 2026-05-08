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
  (res4 :: ["id" := Int64], tIns4) <- insertSch (MyAnn "projects") conn
    ["ownerId" =: (1 :: Int64) :. "title" =: ("pg-schema" :: Text)
      :. "status" =: Project_status_active
      :. "tags" =: pgArr' ["db" :: Text, "haskell"]
    , "ownerId" =: 1 :. "title" =: "tutorial"
      :. "status" =: Project_status_draft :. "tags" =: pgArr' ["learning"]]
  putStrLn $ "\ninsert text: " <> T.unpack tIns4
  putStrLn $ "inserted: " <> show res4
{-
insert text: insert into tut.projects(owner_id,title,status,tags) values (?,?,?,?) returning id
inserted: [PgTag {unPgTag = 1},PgTag {unPgTag = 2}]
-}
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
  let
    (tUpd5 :: Text, updParams5) = updateText_ (MyAnn "projects")
      @("status" := PGEnum Sch ("tut" ->> "project_status")) ("id" =? (2 :: Int64))
  putStrLn $ "update text: " <> T.unpack tUpd5
  putStrLn $ "update params: " <> show updParams5
{-
update text: update tut.projects t0 set status = ? where t0.id = ?
update params: [SomeToField 2]
-}
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
(cnt5, tDel5) <- deleteByCond (MyAnn "projects") conn $ "id" =? (2 :: Int64)
putStrLn $ "\ndelete text: " <> T.unpack (fst tDel5)
putStrLn $ "delete params: " <> show (snd tDel5)
putStrLn $ "deleted: " <> show cnt5 <> " rows"
{-
delete text: delete from tut.projects t0 where t0.id = ?
delete params: [SomeToField 2]
deleted: 1 rows
-}
```

## Операции со связанными таблицами

Pg-schema позволяет проводить операции сразу с несколькими связанными таблицами. Это набор операций: `insertJSON`, `insertJSON_`, `upsertJSON`, `upsertJSON_`. При этом операция осуществляется как единая операция в базе данных. Корректность полей и связей проверяется во время компиляции.

Во всех этих операциях входные и выходные данные неявно сериализуются в json-объекты. В базе данных для выполнения операции создается временная функция (для версий с подчеркиванием - процедура) для обработки этих объектов.

Операции `insert` и `upsert` отличаются только ограничениями, накладываемыми на входные данные:
- для `insert` данные должны содержать все обязательные поля, для которых нет дефолтных значений и которые не получаются из ссылочных ограничений. Так, при вставке данных в родительскую и дочернюю таблицу, идентификаторы родительской таблицы могут генерироваться базой данных при вставке и использоваться при вставке в дочернюю таблицу.
- для выполнения `upsert` того же ограничения достаточно, но также возможны данные, у которых есть не все обязательные поля, зато есть все ключевые значения (части ключа могут быть получены из ссылочного ограничения).

Выходные структуры должны быть произвольными поддеревьями входных структур.

При исполнении запроса реализуется такой алгоритм:

- На каждом уровне дерева код анализирует входные поля текущей таблицы и вычисляет,
  какую операцию выполнить: `UPDATE`, `UPSERT` или `INSERT`.
- Если есть информация о ключе и видно, что обязательные поля неполные, выбирается `UPDATE` по ключу.
- Если есть все обязательные поля и ключ (с учетом полей, пришедших из parent-связей), то выбирается `UPSERT` (т.е. попытка `insert` с `on conflict update` ).
- Во всех остальных случаях выполняется обычный `INSERT`.
- После этого (в той же функции) обрабатываются вложенные массивы детей, и алгоритм
  рекурсивно повторяется для дочерних таблиц.
