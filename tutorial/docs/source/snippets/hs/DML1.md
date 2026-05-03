```haskell
data User = MkUser
  { name :: Text
  , email :: Maybe Text
  } deriving (Show, Generic)
...

do
  conn <- connectPostgreSQL "dbname=tutorial"
  (cnt, tIns) <- insertSch_ (MyAnn "users") conn [MkUser "Benjy" Nothing]
  (res, (tSel, selParams)) <- selectSch (MyAnn "users") @User conn qpEmpty
  putStrLn $ "\ninsert text: " <> T.unpack tIns
  putStrLn $ "inserted " <> show cnt <> " rows"
  putStrLn $ "select text: " <> T.unpack tSel
  putStrLn $ "select params: " <> show selParams
  putStrLn $ "selected rows: " <> show res
```

```
insert text: insert into tut.users(name,email) values (?,?)
inserted 1 rows
select text: select t0.name "name",t0.email "email" from tut.users t0
select params: []
selected rows: [MkUser {name = "Benjy", email = Nothing}]
```
