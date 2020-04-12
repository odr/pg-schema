module Database.PostgreSQL.DML.Insert where

import Control.Monad.RWS
import Data.Bifunctor
import Data.String
import Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int

import Database.PostgreSQL.DB
import Database.Schema.Def
import Database.Schema.Rec
import PgSchema.Util


-- TODO: Insert tree
insertSch
  :: forall sch t r r'
  . ( AllMandatory sch t r, CQueryRecord PG sch t r
    , CQueryRecord PG sch t r', ToRow r, FromRow r' )
  => Connection -> [r] -> IO [r']
insertSch conn = returning conn (insertText @sch @t @r @r')

insertSch_
  :: forall sch t r
  . (AllMandatory sch t r, CQueryRecord PG sch t r, ToRow r)
  => Connection -> [r] -> IO Int64
insertSch_ conn = executeMany conn (insertText_ @sch @t @r)

insertText
  :: forall sch t r r'
  . (AllMandatory sch t r, CQueryRecord PG sch t r, CQueryRecord PG sch t r')
  => Query
insertText = insertText_ @sch @t @r `mappend` " returning " `mappend` fs'
  where
    qr' = getQueryRecord @PG @sch @t @r'
    fs' = fromText
      $ T.intercalate "," [ dbn | (FieldPlain _ dbn _) <- queryFields qr']

insertText_
  :: forall sch t r. CQueryRecord PG sch t r => Query
insertText_ = "insert into " `mappend` tn
  `mappend` "(" `mappend` fs `mappend` ") values (" `mappend` qs
  `mappend` ")"
  where
    qr = getQueryRecord @PG @sch @t @r
    (fs,qs) = bimap inter inter
      $ unzip [ (dbn,"?") | (FieldPlain _ dbn _) <- queryFields qr]
    toQ = fromText
    tn = toQ $ qualName $ tableName qr
    inter = toQ . T.intercalate ","

-- insertText
--   :: forall sch t r r'
--   . (AllMandatory sch t r, CQueryRecord PG sch t r, CQueryRecord PG sch t r')
--   => Text
-- insertText = undefined
--   where
--     qrData = getQueryRecord @PG @sch @t @r
--     qrRes  = getQueryRecord @PG @sch @t @r'
--     dataFlds = queryFields qrData
--
--


type MonadInsert m = MonadRWS (Maybe Int) () Int m

insertM :: MonadInsert m => QueryRecord -> QueryRecord -> [r] -> m Text
insertM qrIn _qrOut _rs = do
  mbParent <- ask
  num <- get
  modify (+1)
  pure $ case mbParent of
    Nothing ->
      "t" <> show' num <> "(_rid," <> fldsIn
        <> ") as (select row_number() over(), " <> fldsIn'
        <> " from (values (" <> quests <> ")) v(" <> fldsIn <> ")"
    Just parentNum ->
      "t" <> show' num <> "(_rid," <> fldsFk <> fldsIn
        <> ") as (select row_number() over()," <> fldsFk <> fldsIn
        <> " from i_t" <> show' parentNum <> "_res ip"
        <> ")"
  -- traverse () $ queryFields qrIn
  where
    fldsIn = T.intercalate ","  $ fst . fldDbName <$> queryFields qrIn
    fldDbName = \case
      FieldPlain _ dbn _ -> (dbn,True)
      FieldTo _ refName _ _ -> (refName, False)
      FieldFrom _ refName _ _ -> (refName, False)
    fldsIn' = T.intercalate ","
      $ ("v."<>) . (\(s,b) -> if b then s else s <> "::jsonb") . fldDbName
      <$> queryFields qrIn
    quests = T.intercalate "," $ "?" <$ queryFields qrIn
    fldsFk = "<fldsFk>"

insertTextM
  :: forall sch tab r r'
  . (CQueryRecord PG sch tab r, CQueryRecord PG sch tab r')
  => [r] -> Text
insertTextM rs = fst $ evalRWS
  ( insertM
    (getQueryRecord @PG @sch @tab @r) (getQueryRecord @PG @sch @tab @r') rs )
  Nothing 0

insertTextM'
  :: forall sch tab r r'
  . (CQueryRecord PG sch tab r, CQueryRecord PG sch tab r')
  => [r] -> Maybe Int -> Int -> Text
insertTextM' rs r = fst . evalRWS
  ( insertM
    (getQueryRecord @PG @sch @tab @r) (getQueryRecord @PG @sch @tab @r') rs ) r

{-
-- flds* - где нужно ':: jsonb'
with
  t1 (_rid, <flds>) as (
    select row_number() over(), <v.flds*>
      from (values <vs,>)
        ) v(<flds)
  ),
  i_t1 (<pkey>,<rflds>) as (
    insert into <tab1>(<flds_plain>)
      select <flds_plain> from <tab1> tab
      returning <rflds>
  ),
  i_t1_res (_rid,<pkey>) as (
    select row_number() over(), <pkey>
      from i_t1
  ),
  orders (_rid, cu_rid, customer_id, day,num, seller_id, state, pos) as (
    select row_number() over(), icr._rid, icr.id, o.day, o.num, o.seller_id, o.state, o.pos
      from i_customers_res icr
	      join customers c on c._rid = icr._rid
        cross join lateral jsonb_to_recordset(c.orders)
          as o(day date, num text, seller_id integer, state sch.order_state, pos jsonb)
  ),
  i_orders (id, customer_id, num, state) as (
    insert into sch.orders(customer_id, day, num, seller_id, state)
      select customer_id, day, num, seller_id, state from orders
      returning id, customer_id, num, state
  ),
  i_orders_res(_rid,id) as (
    select row_number() over(), id
      from i_orders
  ),
  poss (ord_rid, order_id, num, article_id, cnt, price) as (
    select orr._rid, orr.id, p.num, p.article_id, p.cnt, p.price
      from i_orders_res orr
        join orders o on o._rid = orr._rid
        cross join lateral jsonb_to_recordset(o.pos)
          as p(num integer, article_id integer, cnt integer, price numeric)
  ),
  i_poss (order_id, num, price) as (
    insert into sch.order_positions(order_id, num, article_id, cnt, price)
      select order_id, num, article_id, cnt, price from poss
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
