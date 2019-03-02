module Main where

import Cli
import Options.Applicative

-- import Database.PostgreSQL.Schema.Schema


main :: IO ()
main = execParser opts >>= print
