-- | Разметка вывода программы для документации.
--
-- Если задана переменная окружения @TUT_SNIPPETS@, 'section' окружает вывод
-- действия маркерами @--8<-- [start:name]@ / @--8<-- [end:name]@.
-- @make tut-run@ сохраняет такой вывод в @tutorial/out/tutorial.txt@,
-- а главы подключают его через @pymdownx.snippets@:
--
-- > --8<-- "out/tutorial.txt:name"
--
-- Без переменной окружения печатается только заголовок секции,
-- чтобы вывод было удобно читать в консоли.
module Tut.Output (section, formatSql, sqlSection) where

import Control.Exception (finally)
import Data.Char (isSpace, toLower)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)

section :: String -> IO a -> IO a
section name act = do
  marked <- maybe False (not . null) <$> lookupEnv "TUT_SNIPPETS"
  if marked
    then do
      putStrLn $ "--8<-- [start:" <> name <> "]"
      act `finally` putStrLn ("--8<-- [end:" <> name <> "]")
    else do
      putStrLn $ "\n--- " <> name
      act

-- | Отформатировать SQL, который генерирует pg-schema, для чтения человеком.
--
-- Каждая часть запроса (@select@, @from@, @where@, @order by@, @values@,
-- @set@, @on conflict@, @returning@, …) — с новой строки, колонки списка
-- @select@ и присваивания в @set@ — по одной в строке, подзапросы — с отступом.
--
-- Это простой форматтер для примеров в документации, а не разбор SQL:
-- он учитывает только скобки, кавычки и ключевые слова. Текст, в котором уже
-- есть переводы строк (например, функции plpgsql для @*JSON@), не меняется.
formatSql :: Text -> Text
formatSql sql
  | T.any (== '\n') (T.strip sql) = sql
  | otherwise = T.pack $ render $ tokens $ T.unpack sql
  where
    -- Лексемы: строки в кавычках, скобки, запятые, пробелы и прочие слова.
    tokens :: String -> [String]
    tokens [] = []
    tokens (c : cs)
      | c `elem` ("'\"" :: String) =
          let (q, rest) = break (== c) cs
          in (c : q <> take 1 rest) : tokens (drop 1 rest)
      | c `elem` ("()," :: String) = [c] : tokens cs
      | isSpace c = " " : tokens (dropWhile isSpace cs)
      | otherwise =
          let (w, rest) = break (\x -> isSpace x || x `elem` ("()',\"" :: String)) (c : cs)
          in w : tokens rest

    render :: [String] -> String
    render = go 0 [] "" 0

    -- Состояние: глубина скобок, стек открытых операторов (глубина, отступ,
    -- текущая часть оператора), предыдущее слово и отступ текущей строки.
    go :: Int -> [(Int, Int, String)] -> String -> Int -> [String] -> String
    go _ _ _ _ [] = ""
    go d sts prev ind (t : ts)
      | t == " " = case ts of
          (n : _) | startsStmt sts (lower n) || breaks d sts prev (lower n) -> go d sts prev ind ts
          _ -> " " <> go d sts prev ind ts
      | startsStmt sts w =
          let ind' = if null sts then 0 else ind + 4
              pre = if null sts then "" else nl ind'
          in pre <> t <> go d ((d, ind', w) : sts) w ind' ts
      | breaks d sts prev w, ((sd, si, _) : rest) <- sts =
          nl si <> t <> go d ((sd, si, w) : rest) w si ts
      | t == ",", ((sd, si, cl) : _) <- sts, sd == d, Just k <- lookup cl listIndent =
          "," <> nl (si + k) <> go d sts prev (si + k) (dropWhile (== " ") ts)
      | t == "(" = t <> go (d + 1) sts prev ind ts
      | t == ")" =
          let sts' = case sts of
                ((sd, _, _) : rest) | sd == d -> rest
                _ -> sts
          in t <> go (d - 1) sts' prev ind ts
      | otherwise = t <> go d sts (if all isSpace t then prev else w) ind ts
      where
        w = lower t

    startsStmt sts w = w `elem` ["select", "insert", "update", "delete"]
      && not (w == "update" && inClause "on" sts)

    -- Перенос перед частью оператора на его же уровне скобок.
    breaks d sts prev w = case sts of
      ((sd, _, cl) : _) -> sd == d && w `elem` clauses
        && not (w == "from" && prev `elem` ["delete", "distinct"])
        && not (w == "set" && cl == "on")
      [] -> False

    inClause c = \case
      ((_, _, cl) : _) -> cl == c
      [] -> False

    clauses =
      [ "from", "where", "order", "group", "having", "limit", "offset"
      , "values", "set", "on", "returning" ]

    -- Отступ элементов списка относительно начала оператора.
    listIndent = [("select", 7), ("set", 4)]

    nl n = "\n" <> replicate n ' '

    lower = map toLower

-- | Отформатированный SQL примера: отдельная секция @<name>-sql@.
-- Глава показывает её в сворачиваемом блоке сразу после вывода примера.
sqlSection :: String -> [Text] -> IO ()
sqlSection name sqls = section (name <> "-sql") $
  putStrLn $ T.unpack $ T.intercalate (T.pack "\n\n") (terminate . formatSql . T.strip <$> sqls)
  where
    terminate q = if T.isSuffixOf (T.pack ";") q then q else q <> T.pack ";"
