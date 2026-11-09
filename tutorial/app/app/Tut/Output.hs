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
module Tut.Output (section) where

import Control.Exception (finally)
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
