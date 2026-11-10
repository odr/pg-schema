# Ошибки компиляции

В предыдущих главах мы не раз нарочно ошибались и смотрели, что ответит
компилятор. Эта глава собирает такие ошибки вместе. Её не обязательно читать
подряд: удобнее вернуться к ней, когда компилятор выдаст непонятное сообщение,
и найти здесь похожее.

Сначала о том, как такие сообщения читать. Ошибка GHC состоит из нескольких
пунктов, и каждый начинается с `•`. Первый пункт — это само сообщение
`pg-schema`: какая таблица, какое поле и что с ним не так. Следующие пункты
(`In a stmt of a 'do' block`, `In the first argument of …`) показывают, где в
коде произошла ошибка, и обычно указывают на вызов операции (`selectSch`,
`insertJSON` и так далее), а не на определение типа. Это естественно: сам тип
строки ничего не знает о базе, и проверяется он только там, где встречается
вместе с аннотацией. Поэтому одна ошибка в типе может дать несколько
одинаковых сообщений — по одному на каждый вызов, где этот тип используется.

Во всех примерах ниже показан только первый пункт сообщения.

## Имена

Самая частая ошибка — опечатка в имени. Если не найдена таблица, сообщение
короткое:

```
• In schema Sch.Sch the table NameNS "tut" "Users" is not defined.
```

Если не найдено поле или связь, компилятор перечисляет все колонки и внешние
ключи таблицы, так что правильное имя обычно видно сразу:

```
• In schema Sch
  for table "tut" ->> "tasks"
  name "titel" is not defined.

  Valid values are:
    Fields: project_id, seq, title, priority, payload, watchers.
    Foreign key constraints: tasks_project_id_fkey, task_events_project_id_seq_fkey.

  Your source or target type or renaimer is probably invalid.
```

Имя в сообщении — это имя после ренеймера. Если вы написали `titel`, а в
сообщении видите что-то неожиданное, дело, скорее всего, в ренеймере: стоит
проверить, во что он превращает это имя. Такое же сообщение бывает и для
имён в условиях и сортировке, и для полей рекордов, и для полей `PgTag`.

Для имени связи в `qPath` сообщение показывает и имя до ренеймера, и
допустимые связи, разделённые по направлению:

```
• In schema Sch
  for table NameNS "tut" "projects"
  relation "tasks_project_fkey" ("tasksProjectFkey" after applying the renamer) is not defined.

  Valid values are:
    Foreign key constraints to parent tables (qPathFromHere or qPath): projects_owner_id_fkey.
    Foreign key constraints from child tables (qPathToHere or qPath): tasks_project_id_fkey.
```

Для связи таблицы с самой собой `qPath` не годится, и компилятор подскажет,
что использовать вместо него (подробнее — в главе [«Select»](04-select.md)):

```
• qPath cannot be used for self-reference relation.
  Use qPathFromHere or qPathToHere.

  Table: NameNS "tut" "users"
  Relation: "users_manager_id_fkey"
```

## Типы полей и NULL

Тип поля в Haskell должен соответствовать типу колонки. Если колонка
допускает `NULL`, поле должно быть `Maybe`, а если не допускает — не должно:

```
• You can't use Maybe for mandatory fields

  Table: NameNS "tut" "users"
  DB Field name: "name"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Maybe Text
```

```
• You have to use Maybe for nullable fields

  Table: NameNS "tut" "users"
  DB Field name: "email"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Text
```

То же правило действует и для значений в условиях: сравнивать nullable-колонку
нужно со значением типа `Maybe`, например `"managerId" =? Just 1`. Сообщение
будет тем же, «You have to use Maybe for nullable fields».

Если же тип Haskell просто не подходит к типу колонки, сообщение менее
дружелюбное:

```
• Could not solve: ‘CanConvert1
                      Sch
                      (NameNS "tut" "users")
                      "email"
                      (NameNS "pg_catalog" "text")
                      (TypDef "S" Nothing '[])
                      Int’
```

Читать его стоит так: для таблицы `tut.users` и поля `email` с типом
`pg_catalog.text` не нашлось способа преобразования в `Int`. Значит, тип поля
нужно поменять на подходящий, здесь — на `Text`.

## Агрегаты

Для агрегатов проверяется тип результата SQL-функции. Например, `avg` по
колонке `integer` возвращает `numeric`, и `Double` к нему не подходит:

```
• Aggregate 'avg' over a field of type 'int4' returns SQL type 'numeric',
  but Haskell type Double is not compatible with it.
  Use Scientific or Rational instead.

  Table: NameNS "tut" "tasks"
  DB Field name: "priority"
  DB Field type: NameNS "pg_catalog" "int4"
  Haskell Field type: Aggr AAvg (Maybe Double)
```

Если `Maybe` нужен, он ставится внутрь агрегата (`Aggr AMax (Maybe Int32)`),
а не снаружи (`Maybe (Aggr' AMax Int32)`):

```
• You can't use Maybe for aggregates. Use Maybe inside Aggr'

  Table: NameNS "tut" "tasks"
  DB Field name: "priority"
  DB Field type: NameNS "pg_catalog" "int4"
  Haskell Field type: Aggr' AMax Int32
```

## Обязательные колонки и ключи

Для вставки во входных данных должны быть все обязательные колонки, то есть
объявленные `NOT NULL` и не имеющие значения по умолчанию:

```
• We can't insert data because not all mandatory fields are present.
  Table: NameNS "tut" "projects"
  Missing mandatory fields: '["title"]
```

Та же ошибка появится после того, как в таблицу добавят обязательную колонку и
перегенерируют `Sch.hs`: компилятор покажет все места, где в эту таблицу
вставляются строки. Ту же ошибку выдаст и `upsertByKey`, если в записи есть
только ключ: строка может оказаться новой, и вставить её будет нечем. Для
изменения строки по одному ключу нужен `updateByKey`.

Операциям, которые изменяют строки по ключу (`updateByKey`, `updateJSON`,
`upsertJSON` для строк без всех обязательных колонок), нужен полный первичный
или уникальный ключ. Если его нет, компилятор перечислит возможные ключи:

```
• We can't upsert data because for table NameNS "tut" "tasks"
  not all mandatory fields are present and no full primary or
  unique key is covered by record fields.
  Missing mandatory fields: ["seq", "title"]
  Possible keys: '[["project_id", "seq"]]
```

Сообщение общее для нескольких операций, поэтому в нём говорится об upsert и
об обязательных колонках, даже если вы вызывали `updateByKey` или
`updateJSON`. Для них важна последняя строка: один из перечисленных ключей
должен целиком оказаться в типе. В операциях со связанными таблицами колонки,
которые приходят от родителя (здесь `project_id`), в типе указывать не нужно.

## Структура запроса

`qLimit`, `qOffset` и `qDistinct` разрешены только в корне запроса и в
дочерних ветках: у строки один родитель, и ограничивать его количество
бессмысленно. Сообщение об этом, к сожалению, не самое понятное:

```
• Couldn't match type ‘pg-schema-0.8.1.1:PgSchema.Schema.RelOne’
                 with ‘pg-schema-0.8.1.1:PgSchema.Schema.RelMany’
    arising from a use of ‘qLimit’
```

Если вы видите `RelOne` и `RelMany`, проверьте, не стоит ли `qLimit`,
`qOffset` или `qDistinct` внутри `qPath` к родителю.

## Вспомогательные функции

`named` проверяет, что имён столько же, сколько значений в строке. Если имён
меньше, последнему имени достались бы сразу несколько значений:

```
• named: there are fewer field names than fields in the row.
    The last name "status" is given to more than one field: PGEnum
                                                              Sch ("tut" ->> "project_status")
                                                            :. PgArr Text
```

`UnsafeCol` проверяет, что знаков `?` в выражении столько же, сколько колонок
в списке:

```
• Count of field names in the list of fields is more then count of question marks in expression.
```

## Мини-checklist после главы

- [ ] Первый пункт сообщения — это сообщение `pg-schema`, остальные
      показывают место в коде.
- [ ] Имя в сообщении — имя после ренеймера.
- [ ] Для `NULL`-колонок нужен `Maybe` и в типе поля, и в значении условия.
- [ ] Сообщение о ключах читается по строке `Possible keys`.
- [ ] `RelOne`/`RelMany` означает `qLimit`, `qOffset` или `qDistinct` в
      ветке к родителю.

## Что дальше

Эта глава пригодится как справочник по сообщениям компилятора. Правила для
типов результата и ограничения `pg-schema` собраны в
[справочнике](appendix-reference.md).
