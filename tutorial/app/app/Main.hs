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
import Data.Time
import Data.Int
import PgSchema.DML
import Sch

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
  (cnt, tIns) <- insertSch_ (MyAnn "users") conn [MkUser "Benjy" Nothing]
  (res, (tSel, selParams)) <- selectSch (MyAnn "users") @User conn qpEmpty
  putStrLn $ "\ninsert text: " <> T.unpack tIns
  putStrLn $ "inserted " <> show cnt <> " rows"
  putStrLn $ "select text: " <> T.unpack tSel
  putStrLn $ "select params: " <> show selParams
  putStrLn $ "selected rows: " <> show res
  {-
inserted 1 rows
selected rows: [MkUser {name = "Bruce", email = Nothing}]
insert text: insert into tut.users(name,email) values (?,?)
select text: select t0.name "name",t0.email "email" from tut.users t0
select params: []
  -}

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
inserted: ["id" =: 2 :. ("createdAt" =: 2026-05-01 18:25:06.8636 UTC :. MkUser {name = "William", email = Just "william@example.com"}),"id" =: 3 :. ("createdAt" =: 2026-05-01 18:25:06.8636 UTC :. MkUser {name = "Bendji", email = Just "bendji@example.com"})]
select text: select t0.name "name" from tut.users t0
selected rows: ["name" =: "Bruce","name" =: "William","name" =: "Bendji"]
select params: []
-}

  (res4 :: ["id" := Int64], tIns4) <- insertSch (MyAnn "projects") conn
    ["ownerId" =: (1 :: Int64) :. "title" =: ("pg-schema" :: Text)
      :. "status" =: Project_status_active
      :. "tags" =: pgArr' ["db" :: Text, "haskell"]
    , "ownerId" =: 1 :. "title" =: "tutorial"
      :. "status" =: Project_status_draft :. "tags" =: pgArr' ["learning"]]
  putStrLn $ "\ninsert text: " <> T.unpack tIns4
  putStrLn $ "inserted: " <> show res4
{-
insert text: insert into tut.projects(owner_id,title,status,tags) values (?,?,?,?) returning id
inserted: ["id" =: 1,"id" =: 2]
-}
  let
    (tUpd5 :: Text, updParams5) = updateText_ (MyAnn "projects")
      @("status" := PGEnum Sch ("tut" ->> "project_status")) ("id" =? (2 :: Int64))
  putStrLn $ "\nupdate text: " <> T.unpack tUpd5
  putStrLn $ "update params: " <> show updParams5
{-
update text: update tut.projects t0 set status = ? where t0.id = ?
update params: [SomeToField 2]
-}

  (cnt5, tDel5) <- deleteByCond (MyAnn "projects") conn $ "id" =? (2 :: Int64)
  putStrLn $ "\ndelete text: " <> T.unpack (fst tDel5)
  putStrLn $ "delete params: " <> show (snd tDel5)
  putStrLn $ "deleted: " <> show cnt5 <> " rows"
{-
delete text: delete from tut.projects t0  where t0.id = ?
delete params: [SomeToField 2]
deleted: 1 rows
-}

  let
    treeIn =
      [ "ownerId" =: (1 :: Int64)
        :. "title" =: ("tree-demo" :: Text)
        :. "status" =: Project_status_active
        :. "tags" =: pgArr' ["nested" :: Text]
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-1" :: Text)
            :. "priority" =: (10 :: Int32)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("created" :: Text)
              ]
          ]
      ]

  -- insertJSON_ (без returning)
  tInsJ0 <- insertJSON_ (MyAnn "projects") conn treeIn
  putStrLn $ "\ninsertJSON_ text: " <> T.unpack tInsJ0

  -- insertJSON (с returning)
  (resJ1 :: ["id" := Int64 :. "tasksProjectIdFkey" := ["projectId" := Int64 :. "seq" := Int32]], tInsJ1) <-
    insertJSON (MyAnn "projects") conn treeIn
  putStrLn $ "\ninsertJSON text: " <> T.unpack tInsJ1
  putStrLn $ "insertJSON result: " <> show resJ1

  -- upsertJSON_ (без returning)
  let
    treeUps =
      [ "id" =: (1 :: Int64)
        :. "tasksProjectIdFkey" =:
          [ "seq" =: (1 :: Int32)
            :. "title" =: ("task-1-updated" :: Text)
            :. "taskEventsProjectIdSeqFkey" =:
              [ "eventNo" =: (1 :: Int32)
                :. "kind" =: ("updated" :: Text)
              ]
          ]
      ]
  tUpsJ0 <- upsertJSON_ (MyAnn "projects") conn treeUps
  putStrLn $ "\nupsertJSON_ text: " <> T.unpack tUpsJ0

  -- upsertJSON (с returning)
  (resUpsJ1 :: ["id" := Int64 :. "tasksProjectIdFkey" := ["seq" := Int32 :. "title" := Text]], tUpsJ1) <-
    upsertJSON (MyAnn "projects") conn treeUps
  putStrLn $ "\nupsertJSON text: " <> T.unpack tUpsJ1
  putStrLn $ "upsertJSON result: " <> show resUpsJ1
