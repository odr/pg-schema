module Cli where

import Data.ByteString as BS
import Data.Semigroup ((<>))
import Data.Text
import Data.Time
import Options.Applicative


data Cli = Cli
  { cliConnectStr :: ByteString
  , cliPeriod     :: DiffTime
  , cliFileName   :: FilePath
  , cliSchema     :: Text }
  deriving Show

cli :: Parser Cli
cli = Cli
  <$> strOption
    ( long "db-conn"
    <> short 'c'
    <> metavar "DBConnStr"
    <> help "database connection string" )
  <*> (realToFrac @Double <$> option auto
    ( long "period"
    <> help "period for checking, s"
    <> short 'p'
    <> showDefault
    <> value 30
    <> metavar "INT" ))
  <*> strOption
    ( long "hsfile"
    <> short 'f'
    <> metavar "FILE"
    <> help "name of source hs-file to write db-hash" )
  <*> strOption
    ( long "db-schema"
    <> short 's'
    <> help "name of schema in PostgreSQL DB to check"
    <> metavar "SCHEMA" )

opts :: ParserInfo Cli
opts = info (cli <**> helper)
  ( fullDesc
  <> progDesc "Service for updating hash db in hs-file on changing of db-struct"
  <> header "pg-schema-checker - an utility for pg-schema infrastructure" )
