{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Глава «На практике»: несколько схем БД и связи, которых нет в базе.
module Tut.Practice (practice) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import PgSchema.DML
import Sch
import Tut.Common
import Tut.Output

-- --8<-- [start:prac-audit-ann]
type AuditAnn t = 'Ann MyRenamer Sch 3 ("audit" ->> t)
-- --8<-- [end:prac-audit-ann]

-- --8<-- [start:prac-tree-types]
type LogIn = "message" := Text

type ProjectWithLog = "ownerId" := Int64 :. "title" := Text
  :. "projectLogProjectIdFkey" := [LogIn]
-- --8<-- [end:prac-tree-types]

-- --8<-- [start:prac-event-types]
type EventIn = "eventNo" := Int32 :. "kind" := Text :. "authorId" := Maybe Int64

type TaskIn = "seq" := Int32 :. "title" := Text
  :. "taskEventsProjectIdSeqFkey" := [EventIn]
-- --8<-- [end:prac-event-types]

practice :: Connection -> IO ()
practice conn = do
  _ <- insertSch_ (MyAnn "users") conn $ named ["name"] ["Alice" :: Text, "Bob"]

  sqls1 <- section "prac-audit-insert" $ do
    -- --8<-- [start:prac-audit-insert]
    (keys, sqlIns) <- insertJSON (MyAnn "projects") conn $ rowsOf @ProjectWithLog
      [ 1 :. "Website" :. [ "created", "owner assigned" ] ]
    print (plains @("id" := Int64 :. "projectLogProjectIdFkey" := ["id" := Int64]) keys)
    -- --8<-- [end:prac-audit-insert]
    pure [sqlIns]
  sqlSection "prac-audit-insert" sqls1

  sqls2 <- section "prac-audit-select" $ do
    -- --8<-- [start:prac-audit-select]
    (rows, (sql, _)) <- selectSch (AuditAnn "project_log")
      @("message" := Text :. "projectLogProjectIdFkey" := ("title" := Text))
      conn $ qRoot do
        qOrderBy [ascf "id"]
    mapM_ print rows
    -- --8<-- [end:prac-audit-select]
    pure [sql]
  sqlSection "prac-audit-select" sqls2

  _ <- insertJSON_ (MyAnn "tasks") conn $ rowsOf @("projectId" := Int64 :. TaskIn)
    [ 1 :. 1 :. "Design" :. [ 1 :. "created" :. Just 1, 2 :. "reviewed" :. Just 2 ]
    , 1 :. 2 :. "Backend" :. [ 1 :. "created" :. Nothing ] ]

  sqls3 <- section "prac-addrel" $ do
    -- --8<-- [start:prac-addrel]
    (rows, (sql, _)) <- selectSch (MyAnn "task_events")
      @( "seq" := Int32 :. "eventNo" := Int32 :. "kind" := Text
      :. "taskEventsAuthor" := Maybe ("name" := Text) )
      conn $ qRoot do
        qOrderBy [ascf "seq", ascf "eventNo"]
    mapM_ print rows
    -- --8<-- [end:prac-addrel]
    pure [sql]
  sqlSection "prac-addrel" sqls3

  sqls4 <- section "prac-addrel-children" $ do
    -- --8<-- [start:prac-addrel-children]
    (rows, (sql, _)) <- selectSch (MyAnn "users")
      @("name" := Text :. "taskEventsAuthor" := ["kind" := Text])
      conn $ qRoot do
        qOrderBy [ascf "name"]
    mapM_ print rows
    -- --8<-- [end:prac-addrel-children]
    pure [sql]
  sqlSection "prac-addrel-children" sqls4
