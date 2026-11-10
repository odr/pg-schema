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
module Tut.Output (section, formatSql) where

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

-- | Отформатировать @select@, который генерирует pg-schema, для чтения
-- человеком: каждая часть запроса (@select@, @from@, @where@, @order by@, …)
-- и каждая колонка списка @select@ — с новой строки, подзапросы — с отступом.
--
-- Это простой форматтер для примеров в документации, а не разбор SQL:
-- он учитывает только скобки, кавычки и ключевые слова.
formatSql :: Text -> Text
formatSql = T.pack . render . tokens . T.unpack
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

    -- Состояние: глубина скобок, стек открытых select (глубина, отступ),
    -- отступ текущей строки.
    render :: [String] -> String
    render = go 0 [] 0
      where
        go :: Int -> [(Int, Int)] -> Int -> [String] -> String
        go _ _ _ [] = ""
        go d sels ind (t : ts)
          | t == " " = case ts of
              (n : _) | breaksBefore d sels (lower n) || lower n == "select" -> go d sels ind ts
              _ -> " " <> go d sels ind ts
          | lower t == "select" =
              let ind' = if null sels then 0 else ind + 4
                  pre = if null sels then "" else nl ind'
              in pre <> t <> go d ((d, ind') : sels) ind' ts
          | breaksBefore d sels (lower t) =
              let ind' = maybe ind snd (topSel sels)
              in nl ind' <> t <> go d sels ind' ts
          | t == "," , Just (sd, si) <- topSel sels, sd == d =
              "," <> nl (si + 7) <> go d sels (si + 7) (dropWhile (== " ") ts)
          | t == "(" = t <> go (d + 1) sels ind ts
          | t == ")" =
              let sels' = case sels of
                    ((sd, _) : rest) | sd == d -> rest
                    _ -> sels
              in t <> go (d - 1) sels' ind ts
          | otherwise = t <> go d sels ind ts

        topSel = \case
          (s : _) -> Just s
          [] -> Nothing

        breaksBefore d sels w = case topSel sels of
          Just (sd, _) -> sd == d && w `elem` clauses
          Nothing -> False

        clauses = ["from", "where", "order", "group", "having", "limit", "offset"]

        nl n = "\n" <> replicate n ' '

    lower = map toLower
