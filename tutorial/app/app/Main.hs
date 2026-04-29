{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple ( connectPostgreSQL )
import GHC.Generics ( Generic )
import GHC.TypeLits (Symbol)
import Data.Text (Text)
import Data.Text qualified as T
import PgSchema.DML
import Sch ( Sch )

data MyRenamer :: Renamer

type family MyRenamerImpl (s :: Symbol) :: Symbol where
  MyRenamerImpl "projectOwner" = "projects_owner_id_fkey"
  MyRenamerImpl s = CamelToSnake s

type instance ApplyRenamer MyRenamer s = MyRenamerImpl s

type MyAnn t = 'Ann MyRenamer Sch 3 ("tut" ->> t)

data User = MkUser
  { name :: Text
  , email :: Maybe Text
  } deriving (Show, Generic)

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=tutorial"
  (cnt, tIns) <- insertSch_ (MyAnn "users") conn [MkUser "Bruce" Nothing]
  (res, (tSel, selParams)) <- selectSch (MyAnn "users") @User conn qpEmpty
  putStrLn $ "inserted " <> show cnt <> " rows"
  putStrLn $ "selected rows: " <> show res
  putStrLn $ "insert text: " <> T.unpack tIns
  putStrLn $ "select text: " <> T.unpack tSel
  putStrLn $ "select params: " <> show selParams
