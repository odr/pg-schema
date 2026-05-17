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
inserted: ["id" =: 1,"id" =: 2]
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
- для выполнения `upsert` того же ограничения достаточно, но также возможны данные, у которых есть не все обязательные поля, зато есть все колонки первичного ключа или подходящего уникального ограничения (сначала UK с колонками `NOT NULL`, при их отсутствии — UK с nullable-колонками; части ключа могут приходить из ссылочного ограничения).

Выходные структуры должны быть произвольными поддеревьями входных структур.

При исполнении `upsertJSON` реализуется такой алгоритм (для `insertJSON` на
каждом узле — только `INSERT`, см. `06-tree-dml`):

- На каждом уровне дерева код анализирует входные поля текущей таблицы и вычисляет, какую операцию выполнить: `UPDATE`, `UPSERT` или `INSERT`.
- Если есть информация о ключе (PK или первый подходящий UK с полным набором колонок в узле) и видно, что обязательные поля неполные, выбирается `UPDATE` по этому ключу.
- Если есть все обязательные поля и полный тот же ключ (с учётом полей из parent-связей), то выбирается `UPSERT` (т.е. попытка `insert` с `on conflict update` по выбранному ключу).
- Во всех остальных случаях выполняется обычный `INSERT`.
- После этого (в той же функции) обрабатываются вложенные массивы детей, и алгоритм рекурсивно повторяется для дочерних таблиц.

Ниже — фрагменты из `tutorial/app/app/Main.hs`: общее дерево `treeIn` для вставки и `treeUps` для апсерта по существующему `id` проекта.

### insertJSON_

Выполняет вставку дерева без `returning`; в БД создаётся временная **процедура** `pg_temp.__ins`. Возвращается только сгенерированный текст (для отладки или логирования).

```haskell
  let
    treeIn =
      [ "ownerId" =: (1 :: Int64)
        :. "title" =: ("tree-demo" :: Text)
        :. "status" =: Project_status_active
        :. "tags" =: pgArr' ["nested" :: Text]
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-1" :: Text)
            :. "priority" =: (10 :: Int32)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("created" :: Text)
              ]
          ]
      ]

  -- insertJSON_ (без returning)
  tInsJ0 <- insertJSON_ (MyAnn "projects") conn treeIn
  putStrLn $ "\ninsertJSON_ text: " <> T.unpack tInsJ0
```

<details>
<summary>Вывод консоли (<code>make tut-from-scratch</code> / <code>make tut-run</code> после <code>tut-db-init</code>)</summary>

```text
insertJSON_ text: create or replace procedure pg_temp.__ins(data_0 jsonb) as $$
declare
  row_0 record;
  id0 int8;
  data_1 jsonb;
  row_1 record;
  project_id1 int8;
  seq1 int4;
  data_2 jsonb;
  row_2 record;
begin
  for row_0 in select * from jsonb_array_elements(data_0)
  loop
    insert into tut.projects(owner_id, title, status, tags)
      values ((row_0.value->>'ownerId')::int8, (row_0.value->>'title')::text, (row_0.value->>'status')::tut.project_status, case when jsonb_typeof(row_0.value->'tags') = 'array' then (select coalesce(array_agg(__x)::text[], '{}') from jsonb_array_elements_text(row_0.value->'tags') __x) else null end)
      returning id into id0;
    data_1 := row_0.value->'tasksProjectIdFkey';
    for row_1 in select * from jsonb_array_elements(data_1)
    loop
      insert into tut.tasks(project_id, seq, title, priority)
        values (id0, (row_1.value->>'seq')::int4, (row_1.value->>'title')::text, (row_1.value->>'priority')::int4)
        on conflict (project_id, seq)
          do update set title = EXCLUDED.title, priority = EXCLUDED.priority
        returning project_id, seq into project_id1, seq1;
      data_2 := row_1.value->'taskEventsProjectIdSeqFkey';
      for row_2 in select * from jsonb_array_elements(data_2)
      loop
        insert into tut.task_events(project_id, seq, event_no, kind)
          values (project_id1, seq1, (row_2.value->>'eventNo')::int4, (row_2.value->>'kind')::text)
          on conflict (project_id, seq, event_no)
            do update set kind = EXCLUDED.kind;
      end loop;
    end loop;
  end loop;

end;
$$ language plpgsql;
```

</details>

### insertJSON

Тот же сценарий вставки, но создаётся **функция** с `returns jsonb`, а в Haskell возвращается результат в виде дерева тегов (по запрошенной форме `returning`).

```haskell
  -- insertJSON (с returning)
  (resJ1 :: ["id" := Int64 :. "tasksProjectIdFkey" := ["projectId" := Int64 :. "seq" := Int32]], tInsJ1) <-
    insertJSON (MyAnn "projects") conn treeIn
  putStrLn $ "\ninsertJSON text: " <> T.unpack tInsJ1
  putStrLn $ "insertJSON result: " <> show resJ1
```

<details>
<summary>Вывод консоли</summary>

```text
insertJSON text: create or replace function pg_temp.__ins(data_0 jsonb) returns jsonb as $$
declare
  row_0 record;
  arr_0 jsonb[];
  id0 int8;
  data_1 jsonb;
  row_1 record;
  arr_1 jsonb[];
  project_id1 int8;
  seq1 int4;
  data_2 jsonb;
  row_2 record;
begin
  arr_0:= '{}';
  for row_0 in select * from jsonb_array_elements(data_0)
  loop
    insert into tut.projects(owner_id, title, status, tags)
      values ((row_0.value->>'ownerId')::int8, (row_0.value->>'title')::text, (row_0.value->>'status')::tut.project_status, case when jsonb_typeof(row_0.value->'tags') = 'array' then (select coalesce(array_agg(__x)::text[], '{}') from jsonb_array_elements_text(row_0.value->'tags') __x) else null end)
      returning id into id0;
    data_1 := row_0.value->'tasksProjectIdFkey';
    arr_1:= '{}';
    for row_1 in select * from jsonb_array_elements(data_1)
    loop
      insert into tut.tasks(project_id, seq, title, priority)
        values (id0, (row_1.value->>'seq')::int4, (row_1.value->>'title')::text, (row_1.value->>'priority')::int4)
        on conflict (project_id, seq)
          do update set title = EXCLUDED.title, priority = EXCLUDED.priority
        returning project_id, seq into project_id1, seq1;
      data_2 := row_1.value->'taskEventsProjectIdSeqFkey';
      for row_2 in select * from jsonb_array_elements(data_2)
      loop
        insert into tut.task_events(project_id, seq, event_no, kind)
          values (project_id1, seq1, (row_2.value->>'eventNo')::int4, (row_2.value->>'kind')::text)
          on conflict (project_id, seq, event_no)
            do update set kind = EXCLUDED.kind;
      end loop;
      arr_1:= array_append(arr_1, jsonb_build_object('projectId', project_id1, 'seq', seq1));
    end loop;
    arr_0:= array_append(arr_0, jsonb_build_object('id', id0, 'tasksProjectIdFkey', to_jsonb(arr_1)));
  end loop;
  return to_jsonb(arr_0);
end;
$$ language plpgsql;
insertJSON result: ["id" =: 4 :. "tasksProjectIdFkey" =: ["projectId" =: 4 :. "seq" =: 1]]
```

</details>

### upsertJSON_

Обновление по ключу родителя (`id` проекта) и вложенных дочерних строк; без `returning` — снова только текст процедуры.

```haskell
  let
    treeUps =
      [ "id" =: (1 :: Int64)
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-1-updated" :: Text)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("updated" :: Text)
              ]
          ]
      ]

  -- upsertJSON_ (без returning)
  tUpsJ0 <- upsertJSON_ (MyAnn "projects") conn treeUps
  putStrLn $ "\nupsertJSON_ text: " <> T.unpack tUpsJ0
```

<details>
<summary>Вывод консоли</summary>

```text
upsertJSON_ text: create or replace procedure pg_temp.__ins(data_0 jsonb) as $$
declare
  row_0 record;
  id0 int8;
  data_1 jsonb;
  row_1 record;
  project_id1 int8;
  seq1 int4;
  data_2 jsonb;
  row_2 record;
begin
  for row_0 in select * from jsonb_array_elements(data_0)
  loop
    update tut.projects
      set id = (row_0.value->>'id')::int8
      where id = (row_0.value->>'id')::int8
      returning id into id0;
    if found then
      data_1 := row_0.value->'tasksProjectIdFkey';
      for row_1 in select * from jsonb_array_elements(data_1)
      loop
        insert into tut.tasks(project_id, seq, title)
          values (id0, (row_1.value->>'seq')::int4, (row_1.value->>'title')::text)
          on conflict (project_id, seq)
            do update set title = EXCLUDED.title
          returning project_id, seq into project_id1, seq1;
        data_2 := row_1.value->'taskEventsProjectIdSeqFkey';
        for row_2 in select * from jsonb_array_elements(data_2)
        loop
          insert into tut.task_events(project_id, seq, event_no, kind)
            values (project_id1, seq1, (row_2.value->>'eventNo')::int4, (row_2.value->>'kind')::text)
            on conflict (project_id, seq, event_no)
              do update set kind = EXCLUDED.kind;
        end loop;
      end loop;
    end if;
  end loop;

end;
$$ language plpgsql;
```

</details>

### upsertJSON

Вариант с `returning` в виде функции и разбором результата в Haskell.

```haskell
  -- upsertJSON (с returning)
  (resUpsJ1 :: ["id" := Int64 :. "tasksProjectIdFkey" := ["seq" := Int32 :. "title" := Text]], tUpsJ1) <-
    upsertJSON (MyAnn "projects") conn treeUps
  putStrLn $ "\nupsertJSON text: " <> T.unpack tUpsJ1
  putStrLn $ "upsertJSON result: " <> show resUpsJ1
```

<details>
<summary>Вывод консоли</summary>

```text
upsertJSON text: create or replace function pg_temp.__ins(data_0 jsonb) returns jsonb as $$
declare
  row_0 record;
  arr_0 jsonb[];
  id0 int8;
  data_1 jsonb;
  row_1 record;
  arr_1 jsonb[];
  project_id1 int8;
  seq1 int4;
  title1 text;
  data_2 jsonb;
  row_2 record;
begin
  arr_0:= '{}';
  for row_0 in select * from jsonb_array_elements(data_0)
  loop
    update tut.projects
      set id = (row_0.value->>'id')::int8
      where id = (row_0.value->>'id')::int8
      returning id into id0;
    if found then
      data_1 := row_0.value->'tasksProjectIdFkey';
      arr_1:= '{}';
      for row_1 in select * from jsonb_array_elements(data_1)
      loop
        insert into tut.tasks(project_id, seq, title)
          values (id0, (row_1.value->>'seq')::int4, (row_1.value->>'title')::text)
          on conflict (project_id, seq)
            do update set title = EXCLUDED.title
          returning project_id, seq, title into project_id1, seq1, title1;
        data_2 := row_1.value->'taskEventsProjectIdSeqFkey';
        for row_2 in select * from jsonb_array_elements(data_2)
        loop
          insert into tut.task_events(project_id, seq, event_no, kind)
            values (project_id1, seq1, (row_2.value->>'eventNo')::int4, (row_2.value->>'kind')::text)
            on conflict (project_id, seq, event_no)
              do update set kind = EXCLUDED.kind;
        end loop;
        arr_1:= array_append(arr_1, jsonb_build_object('seq', seq1, 'title', title1));
      end loop;
      arr_0:= array_append(arr_0, jsonb_build_object('id', id0, 'tasksProjectIdFkey', to_jsonb(arr_1)));
    end if;
  end loop;
  return to_jsonb(arr_0);
end;
$$ language plpgsql;
upsertJSON result: ["id" =: 1 :. "tasksProjectIdFkey" =: ["seq" =: 1 :. "title" =: "task-1-updated"]]
```

</details>

