# Рабочая модель pg-schema

В этой главе мы опишем основной pipeline при использовании библиотеки и увидим, какие свойства запросов проверяются в compile-time.

## Базовый pipeline

Работа с `pg-schema` состоит из двух этапов:

1. **Изменение схемы БД и генерация описания на уровне типов Хаскеля (Схема БД -> `Sch.hs`)**
   Генератор читает metadata PostgreSQL и строит type-level описание.
2. **Использование описания схемы для генерации и выполнения DML-операций**
   Типы аргументов и результата определяют генерируемую команду SQL. Система типов гарантирует, что данные соответствуют структуре БД.

## Генерация схемы БД

### Запуск генерации

--8<-- "source/snippets/hs/generator-main.md"

Для генерации описания мы делаем отдельное приложение. Мы можем сгенерировать одну или несколько схем. Каждая схема генерируется функцией `updateSchemaFile`.
Состав схемы определяется последним параметром:

--8<-- "source/snippets/hs/GenNames.md"

Мы можем включить в нашу схему
- полные схемы БД
- произвольный набор таблиц
- дополнительные отношения между таблицами

При этом в сгенерированном файле мы получим информацию обо всех выбранных таблицах (из схем БД или непосредственно), информацию о полях этих таблиц, все связи между таблицами (FK) и все используемые типы, в том числе enum-типы.

В некоторых (редких) случаях мы можем также задать дополнительные отношения между таблицами. Лучше это делать в самой БД, но это не всегда возможно по той или иной причине.

На практике использовались схемы с 30-50 таблицами.

Для больших баз есть смысл разбить их на несколько связанных схем. Ничто не мешает включить одну таблицу БД в разные схемы.

### Результат генерации

:mag: _Этот раздел можно пропустить при первом чтении_

В результате мы получаем hs-файл. В нем определен ненаселенный тип `Sch` и, среди прочего, инстанс класса `CSchema`:

--8<-- "source/snippets/hs/CSchema.md"

Как видим, здесь перечислены все наши таблицы и типы данных, используемые в нашей схеме, включая `"tut" ->> "project_status"`. (Оператор `->>` применяется для формирования имен с указанием схемы БД - как на уровне типов, так и на уровне значений)

Контекст класса `CSchema` (как и прочих классов, описывающих схему БД) обеспечивает возможность переводить описание БД с уровня типов на уровень значений при помощи `Data.Singletons.demote`.

Кроме `CSchema` сгенерированный файл содержит описания таблиц (набор полей, первичный и уникальные ключи):

--8<-- "source/snippets/hs/CTabDef.md"

А также описания типов и связей между таблицами. Особенно существенным является инстанс `CDBFieldInfo`:

--8<-- "source/snippets/hs/CDBFieldInfo.md"

Здесь имена, связанные с таблицей классифицируются на

- поля таблицы (`'RFPlain`)
- ссылки (FK constraints) на другие таблицы (`'RFFromHere`)
- ссылки на таблицу из других таблиц (`'RFToHere`)

Использование closed type families позволяет формировать подробную информацию об ошибках.

Для типов enum генерируются инстансы data family `PGEnum`:

--8<-- "source/snippets/hs/Enum.md"

Принцип генерации здесь достаточно очевиден.

## DML-операции

### Аннотация

Все DML операции (включая Select-запросы) используют аннотации `Ann` в качестве Required type argument:

--8<-- "source/snippets/hs/Ann.md"

Ренеймер преобразует имена, используемые в приложении в имена БД. Ренеймер делается по такому шаблону:

--8<-- "source/snippets/hs/MyRenamer.md"

(Для удобства `pg-schema` реализует type family `CamelToSnake`).

Далее, аннотация содержит тип схемы (`annSch`), внутри которой мы производим DML-операцию.

Глубина (`annDepth`) применяется при древообразных запросах для указания максимальной глубины дерева. Основное назначение - защита от бесконечных циклов в compile-time. На практике, глубины 3-5 бывает достаточно.

Последний параметр - имя базовой для операции таблицы (для древообразных операциях - корень дерева).

Часто удобно сделать одну или несколько аннотаций для схемы, зафиксировав ренеймер, схему, глубину и, возможно, схему БД. Оставив только имя таблицы в качестве параметра. В нашем случае:

--8<-- "source/snippets/hs/MyAnn.md"

### DML

Простейший пример добавления и чтения данных:

```haskell
data User = MkUser
  { name :: Text
  , email :: Maybe Text
  } deriving (Show, Generic)
...

do
  conn <- connectPostgreSQL "dbname=tutorial"
  (cnt, tIns) <- insertSch_ (MyAnn "users") conn [MkUser "Benjy" Nothing]
  (res, (tSel, selParams)) <- selectSch (MyAnn "users") @User conn qpEmpty
  putStrLn $ "\ninsert text: " <> T.unpack tIns
  putStrLn $ "inserted " <> show cnt <> " rows"
  putStrLn $ "select text: " <> T.unpack tSel
  putStrLn $ "select params: " <> show selParams
  putStrLn $ "selected rows: " <> show res
```

```
insert text: insert into tut.users(name,email) values (?,?)
inserted 1 rows
select text: select t0.name "name",t0.email "email" from tut.users t0
select params: []
selected rows: [MkUser {name = "Benjy", email = Nothing}]
```

В результате мы получаем ту же запись, которую добавили в БД. Кроме результата операции мы получаем текст запроса и список параметров запроса (пустой в данном случае).

:zap: На что стоит обратить внимание:

- Тип `User` содержит не все поля БД. SQL-операция записывает/получает только эти поля. Т.е. _вид операции зависит от input/output структуры данных_
- Для выполнения DML-операций достаточно инстанса `Generic`.
- Типы и опциональность полей у нас соответствуют типам БД и их nullability.

:thinking_face: Что будет в случае ошибки?

- Поменяем `MyAnn "users"` на `MyAnn "Users"`
```
• In schema Sch the table NameNS "tut" "Users" is not defined.
```

- Поменяем `email` на `mail`
```
• In schema Sch
  for table "tut" ->> "users"
  name "mail" is not defined.

  Valid values are:
    Fields: id, name, email, created_at.
    Foreign key constraints: projects_owner_id_fkey.

  Your source or target type or renaimer is probably invalid.
```

- Сделаем `name :: Maybe Text`
```
• You can't use Maybe for mandatory fields

  Table: NameNS "tut" "users"
  DB Field name: "name"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Maybe Text
```

- Сделаем `email :: Text`
```
• You have to use Maybe for nullable fields

  Table: NameNS "tut" "users"
  DB Field name: "email"
  DB Field type: NameNS "pg_catalog" "text"
  Haskell Field type: Text
```

- Сделаем `email :: Maybe Int`
```
• Could not solve: ‘CanConvert1
                      Sch
                      (NameNS "tut" "users")
                      "email"
                      (NameNS "pg_catalog" "text")
                      (TypDef "S" Nothing '[])
                      Int’
    arising from a use of ‘insertSch_’
```

Последнее не так очевидно, но все же понятно, что в схеме `Sch` в таблице `tut.users` поле `email` имеет тип `pg_catalog.text`, что не соответствует типу `Int`

## Использование PgTag

В примерах выше мы использовали обычный Haskell record с инстансом Generic. Это бывает не всегда удобно. Поэтому есть альтернатива - `PgTag`:

--8<-- "source/snippets/hs/PgTag.md"

`PgTag` это аналог известного типа `PgTagged`, но с другими инстансами.

Для удобства введены операторы `(:=)` (на уровне типов) и `(=:)` (на уровне значений).

При помощи тегов мы описываем поля и соединяем их при помощи оператора `(:.)` из пакета `postgresql-simple`. Мы также можем использовать `PgTag`-подход вместе с `Generic`-подходом.

```haskell
  (res2 :: ["id" := Int64 :. "createdAt" := UTCTime :. User], tIns2) <-
    insertSch (MyAnn "users") conn
      [ MkUser "Quentin" (Just "quentin@example.com")
      , MkUser "Jason" (Just "jason@example.com") ]
  (res3 :: ["name" := Text], (tSel3, selParams3)) <- selectSch (MyAnn "users") conn qpEmpty
  putStrLn $ "\ninsert text: " <> T.unpack tIns2
  putStrLn $ "inserted: " <> show res2
  putStrLn $ "select text: " <> T.unpack tSel3
  putStrLn $ "select params: " <> show selParams3
  putStrLn $ "selected rows: " <> show res3
{-
insert text: insert into tut.users(name,email) values (?,?) returning id,created_at,name,email
inserted: ["id" =: 2 :. ("createdAt" =: 2026-05-02 12:23:32.344437 UTC :. MkUser {name = "Quentin", email = Just "quentin@example.com"}),"id" =: 3 :. ("createdAt" =: 2026-05-02 12:23:32.344437 UTC :. MkUser {name = "Jason", email = Just "jason@example.com"})]
select text: select t0.name "name" from tut.users t0
select params: []
selected rows: ["name" =: "Benjy","name" =: "Quentin","name" =: "Jason"]
-}
```

Обратите внимание, здесь мы получаем результат вставки (`insert ... returning`). Причем набор полей в ответе определяется типом результата.
Также можно заметить, что мы пишем `createdAt`, а не `created_at`. Имена полей преобразуются ренеймером. (Т.е. с нашим ренеймером мы можем использовать любой вариант).

:thinking_face: A что будет, если написать `createdAt1`?

Ошибка:
```
• In schema Sch
  for table "tut" ->> "users"
  name "created_at1" is not defined.

  Valid values are:
    Fields: id, name, email, created_at.
    Foreign key constraints: projects_owner_id_fkey.

  Your source or target type or renaimer is probably invalid.
```

Заметим, что в тексте ошибки указано имя после ренейминга.

В следующей главе мы увидим более продвинутые способы манипуляции данными в `pg-schema`.
