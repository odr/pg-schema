# pg-schema-tutorial

Type provider from PostgreSQL. With some batteries.

## Installation

You have to have [postgresql](https://www.postgresql.org/) and [stack](https://docs.haskellstack.org/en/stable/README/) installed before. Than
```
> git clone git@github.com:odr/pg-schema.git
> cd pg-schema-tutorial
> cd sql && ./db && cd ..
> stack install pg-schema-tutorial

```
NB! `db` script will drop if exists and create database `schema_test`

## Brief
Using this package you could

* get ~~all~~ important information from PostgreSQL DB on haskell type level
through TH
* define complex record types in accordance with schema db including referencing
and referenced data
* getting effective select query to populate such records
* ~~getting effective insert/update/delete command to save such records in db~~
(not implemented yet)

A number of possible problems with db-interaction could be resolved in
compile time.

Compilation time for small database (~60 tables) is not bad
(say 10 seconds with -O0).
I suppose that is acceptable for a medium to big databases as well.

## Tutorial

We will use a set of tables which is described in [this file](sql/create.sql)
Many GHC-extensions should be enabled. I use the following
(maybe some of them is not necessary):
```haskell
  AllowAmbiguousTypes
, ConstraintKinds
, DataKinds
, DeriveGeneric
, EmptyCase
, ExistentialQuantification
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
, GADTs
, GeneralizedNewtypeDeriving
, InstanceSigs
, LambdaCase
, MultiParamTypeClasses
, OverloadedStrings
, QuasiQuotes
, RecordWildCards
, ScopedTypeVariables
, TemplateHaskell
, TupleSections
, TypeApplications
, TypeFamilies
, TypeInType
, TypeOperators
```

### TL;DR

```haskell
ghci> import PgSchema

ghci> {mkSchema "dbname=schema_test user=postgres" "Tutorial" (GenNames ["sch"] [])}

ghci> { data Company = Company { name :: Text, address_id :: Maybe Int32 } deriving (Eq, Show, Generic); schemaRec id ''Company }

ghci> { data Article = Article { name :: Text, code :: Maybe Text } deriving (Eq, Show, Generic); schemaRec id ''Article }

ghci> import Data.Fixed
ghci> { data OrdPos = OrdPos { num :: Int32, opos_article :: Article, cnt :: Int32, price :: Centi } deriving (Eq, Show, Generic); schemaRec id ''OrdPos }

ghci> import Data.Time
ghci> import Database.Types.SchList
ghci> { data Order = Order { day :: Day, num :: Text, ord_seller :: Company, opos_order :: SchList OrdPos, state :: Maybe (PGEnum Tutorial ('NameNS "sch" "order_state")) } deriving (Eq, Show, Generic); schemaRec id ''Order }

ghci> instance CQueryRecord PG Tutorial ('NameNS "sch" "companies") Company
ghci> instance CQueryRecord PG Tutorial ('NameNS "sch" "articles") Article
ghci> instance CQueryRecord PG Tutorial ('NameNS "sch" "order_positions") OrdPos
ghci> instance CQueryRecord PG Tutorial ('NameNS "sch" "orders") Order

ghci> instance FromJSON Company
ghci> instance FromField Company where fromField = fromJSONField
ghci> instance FromJSON Article
ghci> instance FromJSON OrdPos
ghci> instance FromRow Order


ghci> conn <- connectPostgreSQL "dbname=schema_test user=postgres"

-- all orders

ghci> mapM_ (print @Order) =<< selectSch @Tutorial @('NameNS "sch" "orders") conn qpEmpty

Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n21", ord_seller = Company {name = "company5", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 3, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 4, price = 18.00},OrdPos {num = 2, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 2, price = 17.00},OrdPos {num = 1, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 3, price = 23.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n2", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 2, price = 28.00}]}, state = Just Order_state_delivered}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article5", code = Just "a5"}, cnt = 4, price = 18.00}]}, state = Just Order_state_booked}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article6", code = Just "a6"}, cnt = 4, price = 18.00}]}, state = Just Order_state_paid}

-- orders where exists position with cnt > 5

ghci> mapM_ (print @Order) =<< selectSch @Tutorial @('NameNS "sch" "orders") conn qpEmpty { qpConds = [rootCond $ pchild @('NameNS "sch" "opos_order") (pcmp @"cnt" >? (5::Int32))] }

Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}

-- all orders sorted decendant by field `num` and with filtered positions.
-- Included only top 2 positions (by cnt) with cnt > 5

ghci> mapM_ (print @Order) =<< selectSch @Tutorial @('NameNS "sch" "orders") conn qpEmpty { qpConds = [cwp @'["opos_order"] (pcmp @"cnt" >? (5::Int32))], qpOrds = [ rootOrd [descf @"num"], owp @'["opos_order"] [descf @"cnt"] ], qpLOs = [lowp @'["opos_order"] (LO (Just 2) Nothing)] }

Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n21", ord_seller = Company {name = "company5", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Nothing}
Order {day = 2018-11-13, num = "n2", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Just Order_state_delivered}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Just Order_state_booked}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Just Order_state_paid}

-- query text for the last query (formatted by hand...):

ghci> selectText @Tutorial @('NameNS "sch" "orders") @Order qpEmpty { qpConds = [cwp @'["opos_order"] (pcmp @"cnt" >? (5::Int32))], qpOrds =[ rootOrd [descf @"num"], owp @'["opos_order"] [descf @"cnt"] ], qpLOs = [lowp @'["opos_order"] (LO (Just 2) Nothing)] }

("select t0.day \"day\", t0.num \"num\"
  , jsonb_build_object('name',t1.name,'address_id',t1.address_id) \"ord_seller\"
  , array_to_json(array(
    select
      jsonb_build_object
        ( 'num',t2.num
        , 'opos_article',jsonb_build_object('name',t3.name,'code',t3.code)
        , 'cnt',t2.cnt
        , 'price',t2.price )
      from sch.order_positions t2 join sch.articles t3 on t2.article_id=t3.id
      where t2.cnt > ? and t2.order_id=t0.id
      order by t2.cnt Desc
      limit 2)) \"opos_order\"
  , t0.state \"state\"
  from sch.orders t0
    join sch.companies t1 on t0.seller_id=t1.id
  order by t0.num Desc"
,[SomeToField 5])


```

### Type provider

We will use ghci for tutorial. So in directory `pg-schema` run
```
> stack ghci
```
All necessary extensions are included in cabal file so they are activated by
default. All modules from `pg-schema` are imported. So run
```haskell
ghci> {mkSchema "dbname=schema_test user=postgres" "Tutorial" (GenNames ["sch"] [])}
ghci> :i Tutorial

data Tutorial 	-- Defined at <interactive>:7:2
instance [safe] CSchema Tutorial -- Defined at <interactive>:7:2
type instance TTypes Tutorial
  = '[ 'NameNS "pg_catalog" "date", 'NameNS "pg_catalog" "int4",
       'NameNS "pg_catalog" "numeric", 'NameNS "pg_catalog" "text",
       'NameNS "pg_catalog" "timestamptz", 'NameNS "sch" "order_state"]
  	-- Defined at <interactive>:7:2
type instance TTabs Tutorial
  = '[ 'NameNS "sch" "addresses", 'NameNS "sch" "articles",
       'NameNS "sch" "cities", 'NameNS "sch" "companies",
       'NameNS "sch" "countries", 'NameNS "sch" "customers",
       'NameNS "sch" "order_positions", 'NameNS "sch" "orders"]
  	-- Defined at <interactive>:7:2

ghci> :info! Tutorial
```
You will see many class instances for Tutorial. The "root" instance is instance
for class `CSchema`.

Note that our "Schema" can include many PG-schemas (namespaces). So we did `mkSchema` for the list of PG-schemas (`["sch"]`). We can also set up a list of tables (as namespace->>tablename) to generate.

There is predefined type `PgCatalog` in module `Database.PostgreSQL.Schema.Catalog`.
It is a type like `Tutorial` but defined "manually". It describes PostgreSQL system schema.

Type `Sch` and instances for it are predefined here in Tutorial application. It is the same as our `Tutorial` type.

With this instance we can get now:
```haskell
ghci> tabInfoMap @Tutorial

-- return Map of all tables with flds and "from" and "to" references
-- tabInfoMap :: forall sch. CSchema sch => M.Map NameNS TabInfo
-- data TabInfo = TabInfo
--   { tiDef  :: TabDef
--   , tiFlds :: M.Map Text FldDef
--   , tiFrom :: M.Map NameNS RelDef
--   , tiTo   :: M.Map NameNS RelDef }
--   deriving (Show, Eq, Ord)

ghci> typDefMap @Tutorial

-- return Map of all types in use.

fromList [("date",TypDef {typCategory = "D", typElem = Nothing, typEnum = []}),("int4",TypDef {typCategory = "N", typElem = Nothing, typEnum = []}),("numeric",TypDef {typCategory = "N", typElem = Nothing, typEnum = []}),("order_state",TypDef {typCategory = "E", typElem = Nothing, typEnum = ["paid","booked","delivered"]}),("text",TypDef {typCategory = "S", typElem = Nothing, typEnum = []}),("timestamptz",TypDef {typCategory = "D", typElem = Nothing, typEnum = []})]

ghci> :t typDefMap @Tutorial

typDefMap @Tutorial :: Data.Map.Internal.Map NameNS TypDef

```

The same info we can get on type level. Just import TypeLits first to make ghci answers more simple.
```haskell
ghci> import GHC.TypeLits

ghci> :kind! TTabs Tutorial

TTabs Tutorial :: [NameNS' Symbol]
= '[ 'NameNS "sch" "addresses", 'NameNS "sch" "articles",
     'NameNS "sch" "cities", 'NameNS "sch" "companies",
     'NameNS "sch" "countries", 'NameNS "sch" "customers",
     'NameNS "sch" "order_positions", 'NameNS "sch" "orders"]

ghci> :kind! TFrom Tutorial ('NameNS "sch" "addresses")

TFrom Tutorial ('NameNS "sch" "addresses") :: [NameNS' Symbol]
= '[ 'NameNS "sch" "address_city"]

ghci> :kind! TTo Tutorial ('NameNS "sch" "addresses")

TTo Tutorial ('NameNS "sch" "addresses") :: [NameNS' Symbol]
= '[ 'NameNS "sch" "comp_addr", 'NameNS "sch" "cust_addr"]

ghci> :kind! TTabDef Tutorial ('NameNS "sch" "addresses")

TTabDef Tutorial ('NameNS "sch" "addresses") :: TabDef' Symbol
= 'TabDef
    '["id", "city_id", "street", "home", "app", "zipcode"] '["id"] '[]

ghci> :kind! TFldDef Tutorial ('NameNS "sch" "addresses") ("street")

TFldDef Tutorial ('NameNS "sch" "addresses") ("street") :: FldDef' Symbol
= 'FldDef ('NameNS "pg_catalog" "text") 'True 'False

```

We can lift this from type level to the level of values using `demote` (it is just `demote` from [singletons](http://hackage.haskell.org/package/singletons) but left to support different singletons-version):

```haskell
ghci> demote @(TFldDef Tutorial ('NameNS "sch" "addresses") ("street"))

FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = True, fdHasDefault = False}
```

The same information we can get about system schema PgCatalog. This information
is defined in library and isn't imported from database really:
```haskell
ghci> :i PgCatalog
data PgCatalog
  	-- Defined at /home/odr/git/pg-schema/pg-schema/src/Database/PostgreSQL/Schema/Catalog.hs:7:1
instance CSchema PgCatalog
  -- Defined at /home/odr/git/pg-schema/pg-schema/src/Database/PostgreSQL/Schema/Catalog.hs:257:10
type instance TTypes PgCatalog
  = '[PGC "oid", PGC "int2", PGC "int2[]", PGC "float4", PGC "bool",
      PGC "name", PGC "char"]
  	-- Defined at /home/odr/git/pg-schema/pg-schema/src/Database/PostgreSQL/Schema/Catalog.hs:268:8
type instance TTabs PgCatalog
  = '[PGC "pg_attribute", PGC "pg_class", PGC "pg_constraint",
      PGC "pg_enum", PGC "pg_namespace", PGC "pg_type"]
  	-- Defined at /home/odr/git/pg-schema/pg-schema/src/Database/PostgreSQL/Schema/Catalog.hs:260:8
...
```

We can now generate dot-description of our schema:
```haskell
ghci> import PgSchema.Gen
ghci> mapM_ T.putStrLn $ T.lines $ genDot @Tutorial False []

digraph G {
  penwidth=2

  subgraph cluster_sch{
    label="sch"
    addresses
    articles
    cities
    companies
    countries
    customers
    order_positions
    orders
  }
  addresses->cities
  cities->countries
  companies->addresses
  customers->addresses
  order_positions->articles
  order_positions->orders
  orders->customers
  orders->companies
  orders->companies
}

```
## Record definitions and selections

Having all this information what we can do now? We can define records and
populate it with data. At first we'll define record and generate some instances (using TH-function `schemaRec`):
```haskell
ghci> import Data.Time
ghci> { data Ord1 = Ord1 { day :: Day, num :: Text, seller_id :: Int32 } deriving (Eq, Show); schemaRec id ''Ord1 }
```
Define type synonym for convenience:
```haskell
ghci> type SchN name = 'NameNS "sch" name
```

Now just define the most important instance:
```haskell
ghci> instance CQueryRecord PG Tutorial (SchN "orders") Ord1
```
And we can get text of query for this record and try to run it now:
```haskell
ghci> selectText @Tutorial @(SchN "orders") @Ord1 qpEmpty
"select t0.day \"day\",t0.num \"num\",t0.seller_id \"seller_id\" from sch.orders t0 "

ghci> conn <- connectPostgreSQL "dbname=schema_test user=postgres"

ghci> (rs :: [Ord1]) <- selectSch @Tutorial @(SchN "orders") conn qpEmpty
<interactive>:73:19: error:
    • No instance for (FromRow Ord1) arising from a use of ‘selectSch’
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(selectSch @Tutorial @(SchN "orders") conn qpEmpty)’
      In a stmt of an interactive GHCi command:
        (rs :: [Ord1]) <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
                          (selectSch @Tutorial @(SchN "orders") conn qpEmpty)
```
Well, we have to define FromRow instance for Ord1. We can do it by hand or use
Generics. I didn't derive Generic yet so have to enable an extension and make instances:
```haskell
ghci> :set -XStandaloneDeriving
ghci> deriving instance Generic Ord1
ghci> instance FromRow Ord1
```

and now:
```haskell
ghci> selectSch @Tutorial @(SchN "orders") conn qpEmpty >>= mapM_ (print @Ord1)
Ord1 {day = 2018-11-11, num = "n22", seller_id = 3}
Ord1 {day = 2018-11-11, num = "n21", seller_id = 5}
Ord1 {day = 2018-11-11, num = "n2", seller_id = 3}
Ord1 {day = 2018-11-11, num = "n1", seller_id = 1}
Ord1 {day = 2018-11-11, num = "n1", seller_id = 1}
```

What will be if we will use a wrong name of field?
```haskell
ghci> { data Ord2 = Ord2 { day :: Day, num :: Text, seler_id :: Int32 } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Ord2 }

ghci> instance CQueryRecord PG Tutorial (SchN "orders") Ord2
<interactive>:88:10: error:
    • No instance for (CQueryFieldT
                         ('FldUnknown "seler_id")
                         PG
                         Tutorial
                         ('NameNS "sch" "orders")
                         ('FieldInfo "seler_id" "seler_id", Int))
        arising from the superclasses of an instance declaration
    • In the instance declaration for
        ‘CQueryRecord PG Tutorial (SchN "orders") Ord2’
```

Well. A message is not so clear. But it is clear that something wrong with field "seler_id" in table "orders".
Let's try to make record with a wrong type:
```haskell
ghci> { data Ord3 = Ord3 { day :: Day, num :: Text, seller_id :: Char } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Ord3 }

ghci> instance CQueryRecord PG Tutorial (SchN "orders") Ord3
<interactive>:90:10: error:
    • No instance for (CanConvert1
                         ('TypDef "N" 'Nothing '[])
                         Tutorial
                         ('NameNS "pg_catalog" "int4")
                         Char)
        arising from the superclasses of an instance declaration
    • In the instance declaration for
        ‘CQueryRecord PG Tutorial (SchN "orders") Ord3’
```
We can see that we try to use "Char" for some field in table "orders" with db-type "int4". Good enough.

Class `CanConvert1` has no methods and we can define it for new types as well.
Now we can check is `seller_id` can be an `Integer`:
```haskell
ghci> { data Ord4 = Ord4 { day :: Day, num :: Text, seller_id :: Integer } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Ord4 }

ghci> instance CQueryRecord PG Tutorial (SchN "orders") Ord4
    • No instance for (CanConvert1
                         ('TypDef "N" 'Nothing '[])
                         Tutorial
                         ('NameNS "pg_catalog" "int4")
                         Integer)
        arising from the superclasses of an instance declaration
    • In the instance declaration for
        ‘CQueryRecord PG Tutorial (SchN "orders") Ord4’
```

No it is impossible. Library suppose that types should be converted in both side.
We can convert from "int4" to Integer but not in turn.

What was generated by `schemaRec` and what is a class `CQueryRecord`?
`schemaRec` generates instances for classes `CRecordInfo` and `CFieldType`.
`CQueryRecord` has a default implementation. All these instances can be used in such way:
```haskell
ghci> -- CRecordInfo

ghci> recordInfo @Ord1
[FieldInfo {fieldName = "day", fieldDbName = "day"},FieldInfo {fieldName = "num", fieldDbName = "num"},FieldInfo {fieldName = "seller_id", fieldDbName = "seller_id"}]

ghci> :kind! TRecordInfo Ord1
TRecordInfo Ord1 :: [FieldInfo' Symbol]
= '[ 'FieldInfo "day" "day", 'FieldInfo "num" "num",
     'FieldInfo "seller_id" "seller_id"]

ghci> -- CFieldInfo

ghci> :kind! TFieldType Ord1 "seller_id"
TFieldType Ord1 "seller_id" :: *
= Int

ghci> -- CQueryRecord

ghci> getQueryRecord @PG @Tutorial @(SchN "orders") @Ord1

QueryRecord {tableName = NameNS {nnsNamespace = "sch", nnsName = "orders"}, queryFields = [FieldPlain "day" "day" (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "date"}, fdNullable = False, fdHasDefault = False}),FieldPlain "num" "num" (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "text"}, fdNullable = False, fdHasDefault = False}),FieldPlain "seller_id" "seller_id" (FldDef {fdType = NameNS {nnsNamespace = "pg_catalog", nnsName = "int4"}, fdNullable = False, fdHasDefault = False})]}
```

## Complex record datatype and selections

We saw how to create simple record and get data from database. But we can make more complicated case.
Each `order` has items (`order_positions`). And probably we want to select `sellers` of order as well.

Beside that order has an enumeration field `state`. We can add it also with type `Maybe (PGEnum Tutorial (SchN "order_state"))` where "order_state" is a name of type in database.

Let's define types:
```haskell
ghci> { data Company = Company { name :: Text, address_id :: Maybe Int32 } deriving (Eq, Show, Generic); schemaRec id ''Company }

ghci> { data Article = Article { name :: Text, code :: Maybe Text } deriving (Eq, Show, Generic); schemaRec ''Article }

ghci> import Data.Fixed
ghci> { data OrdPos = OrdPos { num :: Int, opos_article :: Article, cnt :: Int32, price :: Centi } deriving (Eq, Show, Generic); schemaRec id ''OrdPos }

ghci> import Data.Time
ghci> { data Order = Order { day :: Day, num :: Text, ord_seller :: Company, opos_order :: SchList OrdPos, state :: Maybe (PGEnum Tutorial (SchN "order_state")) } deriving (Eq, Show, Generic); schemaRec id ''Order }

ghci> instance CQueryRecord PG Tutorial (SchN "companies") Company
ghci> instance CQueryRecord PG Tutorial (SchN "articles") Article
ghci> instance CQueryRecord PG Tutorial (SchN "order_positions") OrdPos
ghci> instance CQueryRecord PG Tutorial (SchN "orders") Order
```

Here `SchList` is a special list to get data from child tables.

Well and now we'll try to get the text of select statement and populate data:
```haskell
ghci> selectText @Tutorial @(SchN "orders") @Order qpEmpty

("select t0.day \"day\",t0.num \"num\",jsonb_build_object('name',t1.name,'address_id',t1.address_id) \"ord_seller\",array_to_json(array(select jsonb_build_object('num',t2.num,'opos_article',jsonb_build_object('name',t3.name,'code',t3.code),'cnt',t2.cnt,'price',t2.price) from sch.order_positions t2 join sch.articles t3 on t2.article_id=t3.id where t2.order_id=t0.id)) \"opos_order\",t0.state \"state\" from sch.orders t0 join sch.companies t1 on t0.seller_id=t1.id",[])

ghci> os :: [Order] <- selectSch @Tutorial @(SchN "orders") conn qpEmpty
<interactive>:21:18: error:
    • No instance for (FromRow Order) arising from a use of ‘selectSch’
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(selectSch @Tutorial @(SchN "orders") conn qpEmpty)’
      In a stmt of an interactive GHCi command:
        os :: [Order] <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
                         (selectSch @Tutorial @(SchN "orders") conn qpEmpty)
```

We forgot to make an instance! It is simple:
```haskell
ghci> instance FromRow Order

<interactive>:22:10: error:
    • No instance for (FromField Company)
        arising from a use of ‘Database.PostgreSQL.Simple.FromRow.$dmfromRow’
      There are instances for similar types:
        instance FromField Main.Company
          -- Defined at /home/odr/git/pg-schema/pg-schema-tutorial/app/Main.hs:70:1
    • In the expression:
        Database.PostgreSQL.Simple.FromRow.$dmfromRow @(Order)
      In an equation for ‘Database.PostgreSQL.Simple.FromRow.fromRow’:
          Database.PostgreSQL.Simple.FromRow.fromRow
            = Database.PostgreSQL.Simple.FromRow.$dmfromRow @(Order)
      In the instance declaration for ‘FromRow Order’
```

Hmm, how to resolve this problem? Look at the text of select statement. We'll get
company as json-object! So we need to make `FromJSON` instance at first.
In GHCi I only know how to run TH in one block with data declaration. So I'll make
default `FromJSON` instance for `Generic` data. In real code TH-deriving will probably
work better (both in compile-time and in run-time).

Then I'll create a simple instance for `FromField`:
```haskell
ghci> instance FromJSON Company

ghci> instance FromField Company where fromField = fromJSONField
```

Let's try again:
```haskell
ghci> instance FromRow Order

<interactive>:29:10: error:
    • No instance for (FromJSON OrdPos)
        arising from a use of ‘Database.PostgreSQL.Simple.FromRow.$dmfromRow’
    • In the expression:
        Database.PostgreSQL.Simple.FromRow.$dmfromRow @(Order)
      In an equation for ‘Database.PostgreSQL.Simple.FromRow.fromRow’:
          Database.PostgreSQL.Simple.FromRow.fromRow
            = Database.PostgreSQL.Simple.FromRow.$dmfromRow @(Order)
      In the instance declaration for ‘FromRow Order’
```

Oh..
```haskell
ghci> instance FromJSON OrdPos

<interactive>:30:10: error:
    • No instance for (FromJSON Article)
        arising from a use of ‘aeson-1.3.1.1:Data.Aeson.Types.FromJSON.$dmparseJSON’
    • In the expression:
        aeson-1.3.1.1:Data.Aeson.Types.FromJSON.$dmparseJSON @(OrdPos)
      In an equation for ‘parseJSON’:
          parseJSON
            = aeson-1.3.1.1:Data.Aeson.Types.FromJSON.$dmparseJSON @(OrdPos)
      In the instance declaration for ‘FromJSON OrdPos’
```

Again...
```haskell
ghci> instance FromJSON Article
ghci> instance FromJSON OrdPos
ghci> instance FromRow Order
```

and now...
```haskell
ghci> selectSch @Tutorial @(SchN "orders") conn qpEmpty >>= mapM_ (print @Order)

Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n21", ord_seller = Company {name = "company5", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 3, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 4, price = 18.00},OrdPos {num = 2, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 2, price = 17.00},OrdPos {num = 1, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 3, price = 23.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n2", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 2, price = 28.00}]}, state = Just Order_state_delivered}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article5", code = Just "a5"}, cnt = 4, price = 18.00}]}, state = Just Order_state_booked}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article6", code = Just "a6"}, cnt = 4, price = 18.00}]}, state = Just Order_state_paid}

```

Isn't it's worth to say "Wow"?

Notice that all these data are getting in one select from db.

In fact this way to get data from database was used also on populating `PgCatalog`
on processing TH-generation of schema. Function `mkSchema` make only three selects
to get all information about tables, relations and types in database.

### Conditions

We can add different `where` conditions and orders and limits/offsets on each part of the data tree.

```haskell
ghci> :{
  mapM_ (print @Order) =<< selectSch @Tutorial @('NameNS "sch" "orders") conn qpEmpty
  { qpConds = [cwp @'["opos_order"] (#cnt >? (5::Int))]
  , qpOrds =
    [ rootOrd [descf @"num"]
    , owp @'["opos_order"] [descf @"cnt"] ]
  , qpLOs = [lowp @'["opos_order"] (LO (Just 2) Nothing)] }
  :}
```

### Generation of schema

Although we use TH here for generation of schema, I recommend to use generation of plain Haskell text. You can see it in `pg-schema-tutorial/Setup.hs` (note about `build-type: Custom` and `custom-setup` in `pg-schema-tutorial.cabal`).

For instances of `CQueryRecord` TH is a single generation method because we need more flexibility here.
