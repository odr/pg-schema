module Database.PostgreSQL.DML.Insert where

-- import Control.Monad
import Control.Monad.RWS
import Data.Bifunctor
-- import Data.Maybe
import Data.Text as T
-- import Data.Traversable
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DB
import Database.Schema.Rec
import Database.Schema.ShowType
import PgSchema.Util


-- TODO: Insert tree
insertSch
  :: forall sch t r r'
  . ( CInsertRecord PG sch t r, CQueryRecord PG sch t r', ToRow r, FromRow r' )
  => Connection -> [r] -> IO [r']
insertSch conn = returning conn (insertText @sch @t @r @r')

insertSch_
  :: forall sch t r. (CInsertRecord PG sch t r, ToRow r)
  => Connection -> [r] -> IO Int64
insertSch_ conn = executeMany conn (insertText_ @sch @t @r)

-- TODO: We can set returning only for "Plain" (and monoidal) fields.
-- It doesn't checked now!
insertText
  :: forall sch t r r'. (CInsertRecord PG sch t r, CQueryRecord PG sch t r')
  => Query
insertText = insertText_ @sch @t @r <> " returning " <> fs'
  where
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromText
      $ T.intercalate "," [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']

insertText_ :: forall sch t r. CInsertRecord PG sch t r => Query
insertText_ = "insert into " <> tn <> "(" <> fs <> ") values (" <> qs <> ")"
  where
    ir = getInsertRecord @PG @sch @t @r
    (fs,qs) = bimap inter inter
      $ unzip [ (ifp.fpDbName,"?") | (IFieldPlain ifp) <- ir.iFields]
    tn = fromText $ qualName ir.iTableName
    inter = fromText . T.intercalate ","

insertText'
  :: forall sch t r r'. (CInsertRecord PG sch t r, CQueryRecord PG sch t r')
  => Query
insertText' = insertText_ @sch @t @r <> " returning " <> fs'
  where
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromText
      $ T.intercalate "," [ qfp.fpDbName | (QFieldPlain qfp) <- qFields qr']

--

type MonadInsert = RWS (Maybe Int) () Int

{-
insertTextM
  :: forall sch t r r'.
    (CInsertRecord PG sch t r, CInsertRecord PG sch t r'
    , SubInsertRecord (TInsertRecord PG sch t r') (TInsertRecord PG sch t r) ~ 'True)
  => [r] -> MonadInsert Text
insertTextM rs = do
  t <- for ir.iFields \case
    IFieldPlain{} -> pure mempty
    IFieldTo ift ->
      local (const $ Just 0) do
        put 1
        insertChildTextM ift $ fromMaybe (error "insertTextM: IMPOSSIBLE!")
          $ listToMaybe $ mapMaybe check ir'.iFields
      where
        check = \case
          IFieldPlain {} -> Nothing
          IFieldTo ift' -> ift' <$ guard (ift'.iftDbName == ift.iftDbName)

  let
    t0 = "with t0(_rid, "<> flds <> ") as (select row_number() over (), "
      <> flds <> "from (values(" <> vals <> ")) v(" <> flds <> ")), "
      <> "i_t0(" <> pk <> ") as (insert into " <> tn <> "(" <> fldsPlain
      <> ") select " <> fldsPain <> "from t0 order by _rid returning" <> pk <> ")"

  undefined t
  where
    ir = getInsertRecord @PG @sch @t @r
    ir' = getInsertRecord @PG @sch @t @r'

    tn = qualName ir.iTableName
    flds = T.intercalate ", " $ mapMaybe getPlainDbName ir.iFields
      where
        getPlainDbName = \case
          IFieldPlain _ dbname _ -> Just dbname
          _ -> Nothing
-}
insertChildTextM  :: InsertFieldTo InsertRecord -> InsertFieldTo InsertRecord -> MonadInsert Text
insertChildTextM = undefined

-- insertM :: MonadInsert m => InsertRecord -> QueryRecord -> [r] -> m Text
-- insertM ir _qrOut _rs = do
--   mbParent <- ask
--   num <- get
--   modify (+1)
--   pure $ case mbParent of
--     Nothing ->
--       "t" <> show' num <> "(_rid," <> fldsIn
--         <> ") as (select row_number() over(), " <> fldsIn'
--         <> " from (values (" <> quests <> ")) v(" <> fldsIn <> ")"
--     Just parentNum ->
--       "t" <> show' num <> "(_rid," <> fldsFk <> fldsIn
--         <> ") as (select row_number() over()," <> fldsFk <> fldsIn
--         <> " from i_t" <> show' parentNum <> "_res ip"
--         <> ")"
--   -- traverse () $ queryFields qrIn
--   where
--     fldsIn = T.intercalate ","  $ fst . fldDbName <$> iFields ir
--     fldDbName = \case
--       IFieldPlain _ dbn _ -> (dbn,True)
--       IFieldTo _ refName _ _ -> (refName, False)
--     fldsIn' = T.intercalate ","
--       $ ("v."<>) . (\(s,b) -> if b then s else s <> "::jsonb") . fldDbName
--       <$> iFields ir
--     quests = T.intercalate "," $ "?" <$ iFields ir
--     fldsFk = "<fldsFk>"

-- insertTextM
--   :: forall sch tab r r'
--   . (CInsertRecord PG sch tab r, CQueryRecord PG sch tab r')
--   => [r] -> Text
-- insertTextM rs = fst $ evalRWS
--   ( insertM
--     (getInsertRecord @PG @sch @tab @r) (getQueryRecord @PG @sch @tab @r') rs )
--   Nothing 0

-- insertTextM'
--   :: forall sch tab r r'
--   . (CInsertRecord PG sch tab r, CQueryRecord PG sch tab r')
--   => [r] -> Maybe Int -> Int -> Text
-- insertTextM' rs r = fst . evalRWS
--   ( insertM
--     (getInsertRecord @PG @sch @tab @r) (getQueryRecord @PG @sch @tab @r') rs ) r

{-
--  t1 (_rid, <flds>) as (
--    select row_number() over(), <v.flds*>
--      from (values <vs,>)
--        ) v(<flds)
--  ),
--  i_t1 (<pkey>,<rflds>) as (
--    insert into <tab1>(<flds_plain>)
--      select <flds_plain> from <tab1> tab
--      returning <rflds>
--  ),
--  i_t1_res (_rid,<pkey>) as (
--    select row_number() over(), <pkey>
--      from i_t1
--  ),
with
  customers (_rid, name, note, orders) as (
	select row_number() over (), name, note, orders
	  from (values
	  	('Ivan', 'Ivanov',
		  '[ { "day": "2025-02-26", "num":"123", "seller_id": 3, "state": "paid"
		  	 , "pos":
			   [ {"num": 1, "article_id": 3, "cnt": 7, "price": 12.1}
			   , {"num": 2, "article_id": 4, "cnt": 2, "price": 15.1} ]
			 }
		   , { "day": "2025-02-27", "num":"124", "seller_id": 2
		  	 , "pos":
			   [ {"num": 1, "article_id": 2, "cnt": 5, "price": 11.1}
			   , {"num": 2, "article_id": 3, "cnt": 4, "price": 153} ]
			 } ]' :: jsonb),
	    ('Petr', 'Petrov',
		  '[ { "day": "2024-02-26", "num":"423", "seller_id": 3, "state": "paid"
		  	 , "pos":
			   [ {"num": 1, "article_id": 3, "cnt": 7, "price": 212.1}
			   , {"num": 2, "article_id": 4, "cnt": 2, "price": 215.1} ]
			 }
		   , { "day": "2023-02-27", "num":"424", "seller_id": 2
		  	 , "pos":
			   [ {"num": 1, "article_id": 2, "cnt": 5, "price": 211.1}
			   , {"num": 2, "article_id": 3, "cnt": 4, "price": 2153} ]
			 } ]' :: jsonb)
		) v(name, note, orders)
  ),
  i_customers(id, name) as (
	insert into sch.customers(name, note)
	  select name, note from customers order by _rid
	  returning id, name
  ),
  orders (_rid, customer_id, day, num, seller_id, state, pos) as (
    select row_number() over(), icr.id, o.day, o.num, o.seller_id, o.state, o.pos
      from (select row_number() over() _rid, id from i_customers) icr
	    join customers c on c._rid = icr._rid
        cross join lateral jsonb_to_recordset(c.orders)
          as o(day date, num text, seller_id integer, state sch.order_state, pos jsonb)
  ),
  i_orders (id, customer_id, num, state) as (
    insert into sch.orders(customer_id, day, num, seller_id, state)
      select customer_id, day, num, seller_id, state from orders order by _rid
      returning id, customer_id, num, state
  ),
  poss (_rid, order_id, num, article_id, cnt, price) as (
    select row_number() over() _rid, ior.id, p.num, p.article_id, p.cnt, p.price
      from (select row_number() over() _rid, id, num, state from i_orders) ior
        join orders o on o._rid = ior._rid
        cross join lateral jsonb_to_recordset(o.pos)
          as p(num integer, article_id integer, cnt integer, price numeric)
  ),
  i_poss (order_id, num, price) as (
    insert into sch.order_positions(order_id, num, article_id, cnt, price)
      select order_id, num, article_id, cnt, price from poss order by _rid
      returning order_id, num, price
  )
  select c.id, c.name
    , array_to_json(array(
      select jsonb_build_object(
        'id', o.id
        ,'num', o.num
        ,'state', o.state
        ,'pos',array_to_json(array(
          select jsonb_build_object(
            'num',p.num
            ,'price',p.price)
            from i_poss p
            where p.order_id = o.id
          ))
          )
        from i_orders o
        where o.customer_id = c.id
        ))
    from i_customers c
-}
