{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (when)
import PgSchema.Generation
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- --8<-- [start:gen-names]
genNames :: GenNames
genNames = GenNames
  { schemas = ["tut", "audit"] -- все таблицы этих схем
  , tables = []                -- и отдельные таблицы других схем (здесь не нужны)
  , addRelations =             -- связи, которых нет в базе
    [ AddRelation
      { name = "task_events_author"
      , from = "tut" ->> "task_events"
      , to = "tut" ->> "users"
      , cols = [("author_id", "id")] } ] }
-- --8<-- [end:gen-names]

-- --8<-- [start:gen-main]
main :: IO ()
main = do
  check <- elem "--check" <$> getArgs
  changed <- updateSchemaFile False "tutorial/app/app/Sch.hs"
    (Left "PG_CONN") -- строка соединения из переменной окружения PG_CONN
    "Sch" -- имя генерируемого модуля
    "Sch" -- имя типа схемы
    genNames
  when (check && changed) $ do
    hPutStrLn stderr "Sch.hs did not match the database and has been regenerated"
    exitFailure
-- --8<-- [end:gen-main]
