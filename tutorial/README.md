# Tutorial: как устроены примеры и документация

Код примеров и их вывод не копируются в главы вручную, а подключаются из
исходников через [`pymdownx.snippets`](https://facelessuser.github.io/pymdown-extensions/extensions/snippets/).
Поэтому текст глав не может разойтись с кодом, а при переводе на английский
переводится только проза.

## Структура

```
tutorial/
  app/app/Main.hs          — запускает примеры всех глав по порядку
  app/app/Tut/Common.hs    — MyRenamer, MyAnn, User (общие для всех глав)
  app/app/Tut/Output.hs    — section: разметка вывода для документации
  app/app/Tut/<Глава>.hs   — примеры одной главы (Basics, FlatDml, TreeDml, …)
  app/generator/Main.hs    — генератор Sch.hs
  out/tutorial.txt         — вывод примеров с маркерами (make tut-run)
  docs/ru, docs/en         — главы
  docs/source/snippets     — фрагменты, которых нет в коде примеров
                             (определения из библиотеки, куски Sch.hs)
```

## Код: именованные секции

В исходнике фрагмент окружается маркерами в комментариях:

```haskell
  section "insert-projects" $ do
    -- --8<-- [start:insert-projects]
    (res4 :: ["id" := Int64], tIns4) <- insertSch (MyAnn "projects") conn ...
    putStrLn $ "inserted: " <> show res4
    -- --8<-- [end:insert-projects]
```

В главе он подключается так (общий отступ снимается, `dedent_subsections: true`):

````markdown
```haskell
--8<-- "app/app/Tut/FlatDml.hs:insert-projects"
```
````

Без имени секции подключается весь файл: `--8<-- "app/generator/Main.hs"`.

Имя секции: латиница в нижнем регистре, цифры, `-` и `_`, начинается с буквы.

## Вывод: секции в `out/tutorial.txt`

`section "name" act` (из `Tut.Output`) при заданной переменной окружения
`TUT_SNIPPETS` печатает вокруг вывода `act` маркеры
`--8<-- [start:name]` / `--8<-- [end:name]`. `make tut-run` пересоздаёт БД,
запускает генератор и приложение и пишет вывод в `out/tutorial.txt`.

В главе:

````markdown
```text
--8<-- "out/tutorial.txt:insert-projects"
```
````

Удобно называть секцию кода и секцию вывода одинаково.

`out/tutorial.txt` стоит хранить в git: тогда документация собирается без
PostgreSQL, а `git diff` после `make tut-run` показывает, как изменился
сгенерированный SQL после правок в библиотеке.

## Новый пример — по шагам

1. Добавить код в модуль главы (или новый `Tut/<Глава>.hs` + строку в
   `other-modules` в `tutorial.cabal` + вызов в `Main.hs`), обернуть в
   `section "имя"` и поставить маркеры `-- --8<-- [start:имя]` / `[end:имя]`.
2. `make tut-run` — проверить, что компилируется, и обновить `out/tutorial.txt`.
3. Подключить код и вывод в главе.
4. `make tut-docs-serve` — mkdocs следит за `app/`, `sql/` и `out/`
   (`watch` в `mkdocs.yml`). При ошибке в имени файла или секции сборка
   падает (`check_paths: true`).

## Чего этот механизм не покрывает

- **Ошибки компиляции** («что будет, если…») по-прежнему вставлены в главы
  вручную: программа с ошибкой не соберётся. Их нужно перепроверять после
  изменений в сообщениях об ошибках библиотеки.
- **Фрагменты из библиотеки и `Sch.hs`** (`Ann`, `PgTag`, `CTabDef`, …) лежат
  в `docs/source/snippets/hs` копиями. `Sch.hs` генерируется, маркеры в него
  не поставить.
