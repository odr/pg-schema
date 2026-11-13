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
`insertJSON` и так далее), а не на определение типа: тип строки проверяется
там, где он встречается вместе с аннотацией. Поэтому одна ошибка в типе может
дать несколько одинаковых сообщений — по одному на каждый вызов, где этот тип
используется. Исправлять её нужно один раз, в определении типа, и все эти
сообщения исчезнут. Найти нужный тип помогают таблица и поле из первого
пункта.

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

  Your source or target type or renamer is probably invalid.
```

И имя в сообщении, и список допустимых имён — это имена в базе. Имя из вашего
кода сначала проходит через ренеймер (из имени Haskell он делает имя в базе),
и в сообщение попадает уже результат. С `titel` это незаметно, а вот если
написать `"ownerID"` вместо `"ownerId"`, ренеймер `CamelToSnake` превратит его
в `owner_i_d`, и компилятор сообщит `name "owner_i_d" is not defined`. Если в
сообщении неожиданное имя, проверьте, во что ренеймер превращает то, что вы
написали. Такое же сообщение бывает и для имён в условиях и сортировке, и для
полей рекордов, и для полей `PgTag`.

Для имени связи в `qPath` сообщение показывает и имя в базе (в кавычках), и
то, что было написано в коде (в скобках), а допустимые связи разделяет по
направлению:

```
• In schema Sch
  for table NameNS "tut" "projects"
  relation "tasks_project_fkey" ("tasksProjectFkey" after applying the renamer) is not defined.

  Valid values are:
    Foreign key constraints to parent tables (qPathFromHere or qPath): projects_owner_id_fkey.
    Foreign key constraints from child tables (qPathToHere or qPath): project_log_project_id_fkey, tasks_project_id_fkey.
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
`pg_catalog.text` не нашлось способа преобразования в `Int` (`TypDef "S" …` —
это описание типа в базе, `S` означает строковый тип). Значит, тип поля нужно
поменять на подходящий, здесь — на `Text`. Какие типы Haskell подходят к
каким типам PostgreSQL, сведено в таблицу в
приложении [«Типы данных»](appendix-types.md).

Похожим образом выглядит ошибка, если тип значения в условии не задан,
например `"priority" <? 50` без аннотации: числовой литерал может быть любого
типа, и компилятор не знает, какой выбрать. В сообщении вместо типа Haskell
будет переменная вида `v0`:

```
• Could not deduce ‘CanConvertMaybe
                      Sch
                      (NameNS "tut" "tasks")
                      "priority"
                      (NameNS "pg_catalog" "int4")
                      False
                      (TypDef "N" Nothing '[])
                      v0’
    arising from a use of ‘<?’
```

Достаточно указать тип: `"priority" <? (50 :: Int32)`.

Если же для рекорда забыли вывести `Generic`, сообщение получается длинным и
непонятным, с внутренними именами `pg-schema` вроде `WalkLevelAnn` и
`GToJSONCols`. Узнать его можно по фрагменту `GHC.Internal.Generics.Rep Task`:
`Rep` — это представление типа через `Generic`, и раз оно не вычисляется,
инстанса нет.

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

Напомним (см. главу [«Select»](04-select.md)), что `Aggr` используется в запросах без группировки и
требует `Maybe` внутри, а `Aggr'` — в запросах с группировкой. Если `Maybe`
нужен, он ставится внутрь агрегата (`Aggr AMax (Maybe Int32)`), а не снаружи
(`Maybe (Aggr' AMax Int32)`):

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
только ключ, а в таблице есть обязательные колонки вне ключа: строка может
оказаться новой, и вставить её будет нечем. Чтобы
изменить строку, указав только ключ, нужен `updateByKey`. (`upsertJSON` в такой
ситуации поступает иначе: строку без всех обязательных колонок он не
вставляет, а только изменяет по ключу, поэтому в его результате и появляется
`Maybe`.)

Изменение строк по ключу — это `updateByKey`, `updateJSON`, а также
`upsertJSON` для строк, в которых есть не все обязательные колонки. Всем им
нужен полный первичный или уникальный ключ. Если его нет, компилятор перечислит возможные ключи:

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

## Форма результата операций со связанными таблицами

В `upsertJSON` и `updateJSON` строка, которая изменяется по ключу, может не
найтись, и тогда на её месте в результате будет `Nothing`. Поэтому тип такой
строки в результате должен быть `Maybe`. Слова «update or key-only upsert» в
сообщении означают как раз это: `updateJSON` или строку `upsertJSON`, в
которой есть только ключ. Для дочерних строк компилятор скажет об этом так:

```
• Returning must use Maybe here (update or key-only upsert).
  At path: '[]
  Table: NameNS "tut" "projects"
  Field: "tasksProjectIdFkey"
  Got: ("seq" := Int32) :. ("priority" := Int32)
```

А для корня `upsertJSON`, в котором нет всех обязательных колонок, сообщение
объясняет причину подробнее:

```
• Upsert returning row must be wrapped in Maybe:
  the input row lacks mandatory fields ["owner_id", "title"]
  so it is only updated by key and a row that is not found is returned as null.
  Table: NameNS "tut" "projects"
  Got: ("id" := Int64)
       :. ("tasksProjectIdFkey"
           := [("seq" := Int32) :. ("title" := Text)])
```

В первом случае исправленный тип выглядит так:
`"tasksProjectIdFkey" := [Maybe ("seq" := Int32 :. "priority" := Int32)]`,
то есть `Maybe` оборачивает каждый элемент списка, а не весь список. Корень
результата `updateJSON` тоже должен быть `Maybe`. А вот у строки, в которой
есть все обязательные колонки, `Maybe` запрещён: такая строка вставляется или
изменяется через `insert … on conflict` и есть в результате всегда. Все эти
правила сведены в таблицу в [справочнике](appendix-reference.md#returning).

## Структура запроса

`qLimit`, `qOffset` и `qDistinct` разрешены только в корне запроса и в
`qPath` к дочерним строкам. В `qPath` к родителю они не имеют смысла: родитель
у строки один, и ограничивать нечего. Сообщение об этом, к сожалению, не самое
понятное:

```
• Couldn't match type ‘pg-schema-0.8.1.1:PgSchema.Schema.RelOne’
                 with ‘pg-schema-0.8.1.1:PgSchema.Schema.RelMany’
    arising from a use of ‘qLimit’
```

Если вы видите `RelOne` и `RelMany`, проверьте, не стоит ли `qLimit`,
`qOffset` или `qDistinct` внутри `qPath` к родителю.

## Вспомогательные функции

`named` из главы [«Изменение данных в одной таблице»](03-flat-dml.md) приписывает список имён полей значениям каждой строки. Он
проверяет, что имён столько же, сколько значений в строке. Если имён меньше,
последнему имени достались бы сразу несколько значений:

```
• named: there are fewer field names than fields in the row.
    The last name "status" is given to more than one field: PGEnum
                                                              Sch ("tut" ->> "project_status")
                                                            :. PgArr Text
```

`UnsafeCol` из главы [«Select»](04-select.md) описывает поле результата произвольным SQL-выражением.
Он проверяет, что знаков `?` в выражении столько же, сколько колонок в списке.
Если колонок больше, сообщение такое (а если больше знаков `?` — аналогичное,
с обратной формулировкой):

```
• Count of field names in the list of fields is more than count of question marks in expression.
```

## Резюме

- Первый пункт сообщения — это сообщение `pg-schema`, остальные
  показывают место в коде.
- Имя в сообщении — имя в базе, то есть после ренеймера.
- Одна ошибка в типе даёт по сообщению на каждый вызов, а исправляется
  один раз.
- Для `NULL`-колонок нужен `Maybe` и в типе поля, и в значении условия.
- Сообщение о ключах читается по строке `Possible keys`.
- `RelOne`/`RelMany` рядом с `qLimit`, `qOffset` или `qDistinct` означает их в
  ветке к родителю.

## Что дальше

Эта глава пригодится как справочник по сообщениям компилятора. О том, как
жить с `pg-schema` в настоящем проекте, где схема базы меняется, рассказывает
глава [«На практике»](07-practice.md), а правила для типов результата и
ограничения собраны в [справочнике](appendix-reference.md).
