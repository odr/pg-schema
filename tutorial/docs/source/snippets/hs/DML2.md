```haskell
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
inserted: [PgTag {unPgTag = 2} :. (PgTag {unPgTag = 2026-05-02 12:23:32.344437 UTC} :. MkUser {name = "Quentin", email = Just "quentin@example.com"}),PgTag {unPgTag = 3} :. (PgTag {unPgTag = 2026-05-02 12:23:32.344437 UTC} :. MkUser {name = "Jason", email = Just "jason@example.com"})]
select text: select t0.name "name" from tut.users t0
select params: []
selected rows: [PgTag {unPgTag = "Benjy"},PgTag {unPgTag = "Quentin"},PgTag {unPgTag = "Jason"}]
-}
```
