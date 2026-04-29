```haskell
data User = MkUser
  { name :: Text
  , email :: Maybe Text
  } deriving (Show, Generic)
...

do
  conn <- connectPostgreSQL "dbname=tutorial"
  (cnt, tIns) <- insertSch_ (MyAnn "users") conn [MkUser "Bruce" Nothing]
  (res, (tSel, selParams)) <- selectSch (MyAnn "users") @User conn qpEmpty
  putStrLn $ "inserted " <> show cnt <> " rows"
  putStrLn $ "selected rows: " <> show res
  putStrLn $ "insert text: " <> T.unpack tIns
  putStrLn $ "select text: " <> T.unpack tSel
  putStrLn $ "select params: " <> show selParams
```

```
inserted 1 rows
selected rows: [MkUser {name = "Bruce", email = Nothing}]
insert text: insert into tut.users(name,email) values (?,?)
select text: select t0.name "name",t0.email "email" from tut.users t0
select params: []
```
