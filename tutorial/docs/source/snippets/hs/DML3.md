```haskell
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
inserted: [PgTag {unPgTag = 1},PgTag {unPgTag = 2}]
-}
```
