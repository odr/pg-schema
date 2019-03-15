# pg-schema-tutorial

Type provider from PostgreSQL. With some batteries.

## Installation

You have to have [postgresql](https://www.postgresql.org/) and [stack](https://docs.haskellstack.org/en/stable/README/) installed before. Than
```
> git clone git@github.com:odr/pg-schema.git
> cd pg-schema
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
(not realised yet)

A number of possible problems with db-interaction could be resolved in
compile time.

Compilation time for small database (~60 tables) is not bad
(say 10 seconds with -O0).
I suppose that is acceptable for a medium to big databases as well.

## Tutorial

We will use a set of tables which is described in [this file](sql/create.sql)
Many GHC-extensions should be enabled. I use the following
(maybe some of them is not necessary):
```
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

```
ghci> import Database.PostgreSQL.Schema.TH

ghci> {mkSchema "dbname=schema_test user=postgres" "Tutorial" "sch"}

ghci> { data Company = Company { name :: Text, address_id :: Maybe Int } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Company }

ghci> { data Article = Article { name :: Text, code :: Maybe Text } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Article }

ghci> import Data.Fixed
ghci> { data OrdPos = OrdPos { num :: Int, opos_article :: Article, cnt :: Int, price :: Centi } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''OrdPos }

ghci> import Data.Time
ghci> import Database.Types.SchList
ghci> { data Order = Order { day :: Day, num :: Text, ord_seller :: Company, opos_order :: SchList OrdPos, state :: Maybe (PGEnum Tutorial "order_state") } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Order }

ghci> instance CQueryRecord PG Tutorial "companies" Company
ghci> instance CQueryRecord PG Tutorial "articles" Article
ghci> instance CQueryRecord PG Tutorial "order_positions" OrdPos
ghci> instance CQueryRecord PG Tutorial "orders" Order

ghci> instance FromJSON Company
ghci> instance FromField Company where fromField = fromJSONField
ghci> instance FromJSON Article
ghci> instance FromJSON OrdPos
ghci> instance FromRow Order


ghci> conn <- connectPostgreSQL "dbname=schema_test user=postgres"

-- all orders

ghci> mapM_ (print @Order) =<< selectSch @Tutorial @"orders" conn qpEmpty
Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n21", ord_seller = Company {name = "company5", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 3, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 4, price = 18.00},OrdPos {num = 2, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 2, price = 17.00},OrdPos {num = 1, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 3, price = 23.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n2", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 2, price = 28.00}]}, state = Just Order_state_delivered}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article5", code = Just "a5"}, cnt = 4, price = 18.00}]}, state = Just Order_state_booked}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article6", code = Just "a6"}, cnt = 4, price = 18.00}]}, state = Just Order_state_paid}

-- orders where exists position with cnt > 5

ghci> mapM_ (print @Order) =<< selectSch @Tutorial @"orders" conn qpEmpty { qpConds = [rootCond $ pchild @"opos_order" (#cnt >? (5::Int))] }
Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}

-- all orders sorted decendant by field `num` and with filtered positions.
-- Included only top 2 positions (by cnt) with cnt > 5

ghci> mapM_ (print @Order) =<< selectSch @Tutorial @"orders" conn qpEmpty
  { qpConds = [cwp @'["opos_order"] (#cnt >? (5::Int))]
  , qpOrds =
    [ rootOrd [descf @"num"]
    , owp @'["opos_order"] [descf @"cnt"] ]
  , qpLOs = [lowp @'["opos_order"] (LO (Just 2) Nothing)] }
Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n21", ord_seller = Company {name = "company5", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Nothing}
Order {day = 2018-11-13, num = "n2", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Just Order_state_delivered}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Just Order_state_booked}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = []}, state = Just Order_state_paid}

-- query text for the last query (formatted by hand...):

ghci> selectText @Tutorial @"orders" @Order qpEmpty
  { qpConds = [cwp @'["opos_order"] (#cnt >? (5::Int))]
  , qpOrds =
    [ rootOrd [descf @"num"]
    , owp @'["opos_order"] [descf @"cnt"] ]
  , qpLOs = [lowp @'["opos_order"] (LO (Just 2) Nothing)] }
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
```
ghci> {data Tutorial; mkSchema "dbname=schema_test user=postgres" ''Tutorial "sch"}
ghci> :i Tutorial
```
You will see many class instances for Tutorial. The "root" instance is instance
for class CSchema:
```
ghci> :i CSchema
class (ToStar (TSchema sch), ToStar (TTabs sch),
       ToStar (TRels sch),
       ToStar (Data.Singletons.Prelude.Base.Map (TTabDefSym1 sch) (TTabs sch)),
       ToStar (Data.Singletons.Prelude.Base.Map (TRelDefSym1 sch) (TRels sch)),
       ToStar (TTabFldDefs sch), ToStar (TTabFlds sch),
       ToStar (TTabDefs sch), ToStar (TTypes sch),
       ToStar (Data.Singletons.Prelude.Base.Map (TTypDefSym1 sch) (TTypes sch))) =>
      CSchema (sch :: k) where
  type family TSchema (sch :: k) :: ghc-prim-0.5.3:GHC.Types.Symbol
  type family TTabs (sch :: k) :: [ghc-prim-0.5.3:GHC.Types.Symbol]
  type family TRels (sch :: k) :: [ghc-prim-0.5.3:GHC.Types.Symbol]
  type family TTypes (sch :: k) :: [ghc-prim-0.5.3:GHC.Types.Symbol]
  	-- Defined at /home/odr/git/pg-schema/src/Database/Schema/Def.hs:144:1
instance CSchema Sch
  -- Defined at /home/odr/git/pg-schema/app/Sch.hs:8:1
instance CSchema PgCatalog
  -- Defined at /home/odr/git/pg-schema/src/Database/PostgreSQL/Schema/Catalog.hs:202:10
instance [safe] CSchema Tutorial -- Defined at <interactive>:2:17
```

Type `PgCatalog` and instances for it are defined in module `Database.PostgreSQL.Schema.Catalog`.

Type `Sch` and instances for it are defined in the sample application included
into package. It is the same as our `Tutorial` type.

With this instance we can get now:
```
ghci> schemaName @Tutorial
"sch"

ghci> tabDefMap @Tutorial
fromList [("addresses",(TabDef {tdFlds = ["id","city_id","street","home","app","zipcode"], tdKey = ["id"], tdUniq = []},fromList [("app",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False}),("city_id",FldDef {fdType = "int4", fdNullable = True, fdHasDefault = False}),("home",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("street",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False}),("zipcode",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False})])),("articles",(TabDef {tdFlds = ["id","name","code","created_at","updated_at"], tdKey = ["id"], tdUniq = [["name"]]},fromList [("code",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False}),("created_at",FldDef {fdType = "timestamptz", fdNullable = False, fdHasDefault = True}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("name",FldDef {fdType = "text", fdNullable = False, fdHasDefault = False}),("updated_at",FldDef {fdType = "timestamptz", fdNullable = True, fdHasDefault = False})])),("cities",(TabDef {tdFlds = ["id","country_id","name"], tdKey = ["id"], tdUniq = []},fromList [("country_id",FldDef {fdType = "int4", fdNullable = True, fdHasDefault = False}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("name",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False})])),("companies",(TabDef {tdFlds = ["id","name","address_id","created_at","updated_at"], tdKey = ["id"], tdUniq = []},fromList [("address_id",FldDef {fdType = "int4", fdNullable = True, fdHasDefault = False}),("created_at",FldDef {fdType = "timestamptz", fdNullable = False, fdHasDefault = True}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("name",FldDef {fdType = "text", fdNullable = False, fdHasDefault = False}),("updated_at",FldDef {fdType = "timestamptz", fdNullable = True, fdHasDefault = False})])),("countries",(TabDef {tdFlds = ["id","name","code"], tdKey = ["id"], tdUniq = []},fromList [("code",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("name",FldDef {fdType = "text", fdNullable = False, fdHasDefault = False})])),("customers",(TabDef {tdFlds = ["id","name","address_id","note","created_at","updated_at"], tdKey = ["id"], tdUniq = []},fromList [("address_id",FldDef {fdType = "int4", fdNullable = True, fdHasDefault = False}),("created_at",FldDef {fdType = "timestamptz", fdNullable = False, fdHasDefault = True}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("name",FldDef {fdType = "text", fdNullable = False, fdHasDefault = False}),("note",FldDef {fdType = "text", fdNullable = True, fdHasDefault = False}),("updated_at",FldDef {fdType = "timestamptz", fdNullable = True, fdHasDefault = False})])),("order_positions",(TabDef {tdFlds = ["order_id","num","article_id","cnt","price"], tdKey = ["order_id","num"], tdUniq = [["order_id","article_id"]]},fromList [("article_id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False}),("cnt",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False}),("num",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False}),("order_id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False}),("price",FldDef {fdType = "numeric", fdNullable = False, fdHasDefault = False})])),("orders",(TabDef {tdFlds = ["id","day","num","customer_id","seller_id","trader_id","state","created_at","updated_at"], tdKey = ["id"], tdUniq = []},fromList [("created_at",FldDef {fdType = "timestamptz", fdNullable = False, fdHasDefault = True}),("customer_id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False}),("day",FldDef {fdType = "date", fdNullable = False, fdHasDefault = False}),("id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = True}),("num",FldDef {fdType = "text", fdNullable = False, fdHasDefault = False}),("seller_id",FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False}),("state",FldDef {fdType = "order_state", fdNullable = True, fdHasDefault = False}),("trader_id",FldDef {fdType = "int4", fdNullable = True, fdHasDefault = False}),("updated_at",FldDef {fdType = "timestamptz", fdNullable = True, fdHasDefault = False})]))]

ghci> relDefMap @Tutorial
fromList [("address_city",RelDef {rdFrom = "addresses", rdTo = "cities", rdCols = [("city_id","id")]}),("city_country",RelDef {rdFrom = "cities", rdTo = "countries", rdCols = [("country_id","id")]}),("comp_addr",RelDef {rdFrom = "companies", rdTo = "addresses", rdCols = [("address_id","id")]}),("cust_addr",RelDef {rdFrom = "customers", rdTo = "addresses", rdCols = [("address_id","id")]}),("opos_article",RelDef {rdFrom = "order_positions", rdTo = "articles", rdCols = [("article_id","id")]}),("opos_order",RelDef {rdFrom = "order_positions", rdTo = "orders", rdCols = [("order_id","id")]}),("ord_cust",RelDef {rdFrom = "orders", rdTo = "customers", rdCols = [("customer_id","id")]}),("ord_seller",RelDef {rdFrom = "orders", rdTo = "companies", rdCols = [("seller_id","id")]}),("ord_trader",RelDef {rdFrom = "orders", rdTo = "companies", rdCols = [("trader_id","id")]})]

ghci> typDefMap @Tutorial
fromList [("date",TypDef {typCategory = "D", typElem = Nothing, typEnum = []}),("int4",TypDef {typCategory = "N", typElem = Nothing, typEnum = []}),("numeric",TypDef {typCategory = "N", typElem = Nothing, typEnum = []}),("order_state",TypDef {typCategory = "E", typElem = Nothing, typEnum = ["paid","booked","delivered"]}),("text",TypDef {typCategory = "S", typElem = Nothing, typEnum = []}),("timestamptz",TypDef {typCategory = "D", typElem = Nothing, typEnum = []})]

```

The same info we can get on type level. Just import TypeLits first to make ghci answers more simple.
```
ghci> import GHC.TypeLits

ghci> :kind! TSchema Tutorial
TSchema Tutorial :: Symbol = "sch"

ghci> :kind! TTabs Tutorial
TTabs Tutorial :: [Symbol]
= '["countries", "cities", "addresses", "customers", "companies",
    "orders", "articles", "order_positions"]

ghci> :kind! TRels Tutorial
TRels Tutorial :: [Symbol]
= '["city_country", "address_city", "cust_addr", "comp_addr",
    "ord_cust", "ord_seller", "ord_trader", "opos_order",
    "opos_article"]
```

For each table in schema we can get an additional information from class `CTabDef`:
```
ghci> :i CTabDef
class (ToStar name, ToStar (TTabDef sch name),
       CFldDefs sch name (TdFlds (TTabDef sch name)),
       CFldDefs sch name (TdKey (TTabDef sch name)),
       CFldDefs2 sch name (TdUniq (TTabDef sch name))) =>
      CTabDef (sch :: k) (name :: Symbol) where
  type family TTabDef (sch :: k) (name :: Symbol) :: TabDefK
...
```

E.g. for table `orders`:
```
ghci> tabName @Tutorial @"orders"
"orders"

ghci> :kind! TTabDef Tutorial "orders"
TTabDef Tutorial "orders" :: TabDef' Symbol
= 'TabDef
    '["id", "day", "num", "customer_id", "seller_id", "trader_id",
      "state", "created_at", "updated_at"]
    '["id"]
    '[]

ghci> toStar @_ @(TTabDef Tutorial "orders")
TabDef {tdFlds = ["id","day","num","customer_id","seller_id","trader_id","state","created_at","updated_at"], tdKey = ["id"], tdUniq = []}


```

We can get information not only about tables but also about relations between them:
```
ghci> :i CRelDef
class (ToStar name, ToStar (TRelDef sch name),
       CTabDef sch (TFromTab sch name), CTabDef sch (TToTab sch name),
       CFldDefs sch (TFromTab sch name) (TFromFlds sch name),
       CFldDefs sch (TToTab sch name) (TToFlds sch name)) =>
      CRelDef (sch :: k) (name :: Symbol) where
  type family TRelDef (sch :: k) (name :: Symbol) :: RelDefK
 ...

```

Then for fields of tables we also can get additional description:
```
ghci> :i CFldDef
class (ToStar fname, ToStar (TFldDef sch tname fname),
       CTypDef sch (FdType (TFldDef sch tname fname))) =>
      CFldDef (sch :: k) (tname :: k1) (fname :: Symbol) where
  type family TFldDef (sch :: k) (tname :: k1) (fname :: Symbol) :: FldDefK
```

In field description there is an information about database type of field.
We can get an information about type also (including e.g. enumerations):
```
ghci> :i CTypDef
class (ToStar name, ToStar (TTypDef sch name)) =>
      CTypDef (sch :: k) (name :: Symbol) where
  type family TTypDef (sch :: k) (name :: Symbol) :: TypDefK

ghci> :kind! TTypDef Tutorial "order_state"
TTypDef Tutorial "order_state" :: TypDef' Symbol
= 'TypDef "E" 'Nothing '["paid", "booked", "delivered"]

```

The same information we can get about system schema PgCatalog. This information
is defined in library and isn't imported from database really:
```
ghci> :kind! TTabs PgCatalog
TTabs PgCatalog :: [Symbol]
= '["pg_attribute", "pg_class", "pg_constraint", "pg_enum", "pg_namespace", "pg_type"]

ghci> :kind! TRels PgCatalog
TRels PgCatalog :: [Symbol]
= '["attribute__class", "attribute__type", "class__namespace",
    "constraint__class", "constraint__fclass", "constraint__namespace",
    "enum__type", "type__namespace"]
```

## Record definitions and selections

Having all this information what we can do now? We can define records and
populate it with data. At first we'll define record and generate some instances (using TH-function `schemaRec`):
```
ghci> import Data.Time
ghci> { data Ord1 = Ord1 { day :: Day, num :: Text, seller_id :: Int } deriving (Eq, Show); schemaRec @Tutorial id ''Ord1 }
```
Now just define the most important instance:
```
ghci> instance CQueryRecord PG Tutorial "orders" Ord1
```
And we can get text of query for this record and try to run it now:
```
ghci> selectText @Tutorial @"orders" @Ord1 []
"select t0.day \"day\",t0.num \"num\",t0.seller_id \"seller_id\" from sch.orders t0 "

ghci> conn <- connectPostgreSQL "dbname=schema_test user=postgres"

ghci> (rs :: [Ord1]) <- selectSch @Tutorial @"orders" conn qpEmpty
<interactive>:73:19: error:
    • No instance for (FromRow Ord1) arising from a use of ‘selectSch’
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(selectSch_ @Tutorial @"orders" conn)’
      In a stmt of an interactive GHCi command:
        (rs :: [Ord1]) <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
                          (selectSch_ @Tutorial @"orders" conn)
```
Well, we have to define FromRow instance for Ord1. We can do it by hand or use
Generics. I didn't derive Generic yet so have to enable an extension and make instances:
```
ghci> :set -XStandaloneDeriving
ghci> deriving instance Generic Ord1
ghci> instance FromRow Ord1
```

and now:
```
ghci> selectSch @Tutorial @"orders" conn qpEmpty >>= mapM_ (print @Ord1)
Ord1 {day = 2018-11-11, num = "n22", seller_id = 3}
Ord1 {day = 2018-11-11, num = "n21", seller_id = 5}
Ord1 {day = 2018-11-11, num = "n2", seller_id = 3}
Ord1 {day = 2018-11-11, num = "n1", seller_id = 1}
Ord1 {day = 2018-11-11, num = "n1", seller_id = 1}
```

What will be if we will use a wrong name of field?
```
ghci> { data Ord2 = Ord2 { day :: Day, num :: Text, seler_id :: Int } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Ord2 }

ghci> instance CQueryRecord PG Tutorial "orders" Ord2
<interactive>:88:10: error:
    • No instance for (CQueryFieldT
                         FldUnknownSym0
                         PG
                         Tutorial
                         "orders"
                         Ord2
                         ('FieldInfo "seler_id" "seler_id"))
        arising from the superclasses of an instance declaration
    • In the instance declaration for
        ‘CQueryRecord PG Tutorial "orders" Ord2’
```

Well. A message is not so clear. But it is clear that something wrong with field "seler_id" in table "orders".
Let's try to make record with a wrong type:
```
ghci> { data Ord3 = Ord3 { day :: Day, num :: Text, seller_id :: Char } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Ord3 }

ghci> instance CQueryRecord PG Tutorial "orders" Ord3
<interactive>:90:10: error:
    • No instance for (CanConvert1
                         ('TypDef "N" 'Nothing '[]) Tutorial "int4" Char)
        arising from the superclasses of an instance declaration
    • In the instance declaration for
        ‘CQueryRecord PG Tutorial "orders" Ord3’
```
We can see that we try to use "Char" for some field in table "orders" with db-type "int4". Good enough.

Class `CanConvert1` has no methods and we can define it for new types as well.
Now we can check is `seller_id` can be an `Integer`:
```
ghci> { data Ord4 = Ord4 { day :: Day, num :: Text, seller_id :: Integer } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Ord4 }

ghci> instance CQueryRecord PG Tutorial "orders" Ord4
    • No instance for (CanConvert1
                         ('TypDef "N" 'Nothing '[]) Tutorial "int4" Integer)
        arising from the superclasses of an instance declaration
    • In the instance declaration for
        ‘CQueryRecord PG Tutorial "orders" Ord4’
```

No it is impossible. Library suppose that types should be converted in both side.
We can convert from "int4" to Integer but not in turn.

What was generated by `schemaRec` and what is a class `CQueryRecord`?
`schemaRec` generates instances for classes `CRecordInfo` and `CFieldType`.
`CQueryRecord` has a default implementation. All these instances can be used in such way:
```
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

ghci> getQueryRecord @PG @Tutorial @"orders" @Ord1
QueryRecord {tableName = "orders", queryFields = [FieldPlain "day" "day" (FldDef {fdType = "date", fdNullable = False, fdHasDefault = False}),FieldPlain "num" "num" (FldDef {fdType = "text", fdNullable = False, fdHasDefault = False}),FieldPlain "seller_id" "seller_id" (FldDef {fdType = "int4", fdNullable = False, fdHasDefault = False})]}
```

## Complex record datatype and selections

We saw how to create simple record and get data from database. But we can make more complicated case.
Each order has items (order_positions). And probably we want to select sellers of order as well.

Beside that order has an enumeration field `state`. We can add it also with type `Maybe (PGEnum Tutorial "order_state")` where "order_state" is a name of type in database.

Let's define types:
```
ghci> { data Company = Company { name :: Text, address_id :: Maybe Int } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Company }

ghci> { data Article = Article { name :: Text, code :: Maybe Text } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Article }

ghci> import Data.Fixed
ghci> { data OrdPos = OrdPos { num :: Int, opos_article :: Article, cnt :: Int, price :: Centi } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''OrdPos }

ghci> import Data.Time
ghci> { data Order = Order { day :: Day, num :: Text, ord_seller :: Company, opos_order :: SchList OrdPos, state :: Maybe (PGEnum Tutorial "order_state") } deriving (Eq, Show, Generic); schemaRec @Tutorial id ''Order }

ghci> instance CQueryRecord PG Tutorial "companies" Company
ghci> instance CQueryRecord PG Tutorial "articles" Article
ghci> instance CQueryRecord PG Tutorial "order_positions" OrdPos
ghci> instance CQueryRecord PG Tutorial "orders" Order
```

Here `SchList` is a special list to get data from child tables.

Well and now we'll try to get the text of select statement and populate data:
```
ghci> selectText @Tutorial @"orders" @Order qpEmpty
("select t0.day \"day\",t0.num \"num\",jsonb_build_object('name',t1.name,'address_id',t1.address_id) \"ord_seller\",array_to_json(array(select jsonb_build_object('num',t2.num,'opos_article',jsonb_build_object('name',t3.name,'code',t3.code),'cnt',t2.cnt,'price',t2.price) from sch.order_positions t2 join sch.articles t3 on t2.article_id=t3.id where t2.order_id=t0.id)) \"opos_order\",t0.state \"state\" from sch.orders t0 join sch.companies t1 on t0.seller_id=t1.id",[])

ghci> os :: [Order] <- selectSch @Tutorial @"orders" conn qpEmpty
<interactive>:21:18: error:
    • No instance for (FromRow Order)
        arising from a use of ‘selectSch_’
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(selectSch_ @Tutorial @"orders" conn)’
      In a stmt of an interactive GHCi command:
        os :: [Order] <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
                         (selectSch_ @Tutorial @"orders" conn)
```

We forgot to make an instance! It is simple:
```
ghci> instance FromRow Order

<interactive>:22:10: error:
    • No instance for (FromField Company)
        arising from a use of ‘Database.PostgreSQL.Simple.FromRow.$dmfromRow’
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
```
ghci> instance FromJSON Company

ghci> instance FromField Company where fromField = fromJSONField
```

Let's try again:
```
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
```
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
```
ghci> instance FromJSON Article
ghci> instance FromJSON OrdPos
ghci> instance FromRow Order
```

and now...
```
ghci> selectSch @Tutorial @"orders" conn qpEmpty >>= mapM_ (print @Order)
Order {day = 2018-11-13, num = "n22", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 7, price = 28.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n21", ord_seller = Company {name = "company5", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 3, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 4, price = 18.00},OrdPos {num = 2, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 2, price = 17.00},OrdPos {num = 1, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 3, price = 23.00}]}, state = Nothing}
Order {day = 2018-11-13, num = "n2", ord_seller = Company {name = "company3", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 2, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 2, price = 10.00},OrdPos {num = 1, opos_article = Article {name = "article3", code = Just "a3"}, cnt = 1, price = 120.00},OrdPos {num = 3, opos_article = Article {name = "article4", code = Just "a4"}, cnt = 2, price = 28.00}]}, state = Just Order_state_delivered}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article5", code = Just "a5"}, cnt = 4, price = 18.00}]}, state = Just Order_state_booked}
Order {day = 2018-11-13, num = "n1", ord_seller = Company {name = "company1", address_id = Nothing}, opos_order = SchList {getSchList = [OrdPos {num = 1, opos_article = Article {name = "article1", code = Just "a1"}, cnt = 1, price = 100.00},OrdPos {num = 2, opos_article = Article {name = "article2", code = Just "a2"}, cnt = 2, price = 50.00},OrdPos {num = 3, opos_article = Article {name = "article6", code = Just "a6"}, cnt = 4, price = 18.00}]}, state = Just Order_state_paid}

```

Isn't it's worth to say "Wow"?

Notice that all these data are getting in one select.

In fact this way to get data from database was used also on populating `PgCatalog`
on processing TH-generation of schema. Function `mkSchema` make only three selects
to get all information about tables, relations and types in database.

### Conditions
