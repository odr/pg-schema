with
  qctry as (
    insert into sch.countries (name, code) values
      ('Russia','RU'),
      ('USA','US'),
      ('Great Britain','GB'),
      ('Israel','IL')
      returning id, code
  ), qcty as (
    insert into sch.cities (name,country_id)
      select v.column1, c.id
        from (values
          ('Москва','RU'),
          ('Spb','RU'),
          ('Kazan','RU'),
          ('ירושלים','IL'),
          ('תל-אביב','IL'),
          ('London','GB'),
          ('New-York','US'),
          ('LA','US')) v
          join qctry c on c.code = v.column2
      returning id, name, country_id
  ), qaddr as (
    insert into sch.addresses(city_id, street)
      select c.id,v.column2
        from (values
          ('Москва','Ордынка'),
          ('Spb','Gorohovaya'),
          ('Spb','Nevskiy'),
          ('תל-אביב','שדרות ירושלמי'),
          ('London','Baker st'),
          ('New-York','5 ave'),
          ('New-York','7 ave')) v
          join qcty c on c.name = v.column1
      returning id, street, city_id
  )
  select qctry.*, qcty.*, qaddr.*
    from qctry
      left outer join qcty on qcty.country_id = qctry.id
      left outer join qaddr on qaddr.city_id = qcty.id;

insert into sch.articles(name, code) values
  ('article1','a1'),('article2','a2'),('article3','a3'),('article4','a4')
  ,('article5','a5'),('article6','a6'),('article7','a7'),('article8','a8');

insert into sch.companies(name) values
  ('company1'),('company2'),('company3'),('company4'),('company5')
  ,('company6'),('company7'),('company8'),('company9');

with
  q1 as (insert into sch.customers(name) values ('cust1') returning id),
  q2 as (
    insert into sch.orders(day,num,customer_id,seller_id,state)
      select now(),'n1',q1.id,1,'paid'
      from q1
      returning id
  ),
  q3 as (
    insert into sch.order_positions (order_id,num,article_id,cnt,price)
      select q2.id, v.*
        from q2
          cross join (values (1,1,1,100),(2,2,2,50),(3,6,4,18)) v
      returning *
  ),
  q4 as (
    insert into sch.orders(day,num,customer_id,seller_id,state)
      select now(),'n1',q1.id,1,'booked'
      from q1
      returning id
  ),
  q5 as (
    insert into sch.order_positions (order_id,num,article_id,cnt,price)
      select q4.id, v.*
        from q4
          cross join (values (1,1,1,100),(2,2,2,50),(3,5,4,18)) v
      returning *
  ),
  q6 as (
    insert into sch.orders(day,num,customer_id,seller_id,state)
      select now(),'n2',q1.id,3,'delivered'
      from q1
      returning id
  ),
  q7 as (
    insert into sch.order_positions (order_id,num,article_id,cnt,price)
      select q6.id, v.*
        from q6
          cross join (values (1,3,1,120),(2,1,2,10),(3,4,2,28)) v
      returning *
  ),
  q8 as (insert into sch.customers(name) values ('cust2') returning id),
  q9 as (
    insert into sch.orders(day,num,customer_id,seller_id)
      select now(),'n21',q8.id,5
      from q8
      returning id
  ),
  q10 as (
    insert into sch.order_positions (order_id,num,article_id,cnt,price)
      select q9.id, v.*
        from q9
          cross join (values (1,4,3,23),(2,3,2,17),(3,2,4,18)) v
      returning *
  ),
  q11 as (
    insert into sch.orders(day,num,customer_id,seller_id)
      select now(),'n22',q8.id,3
      from q8
      returning id
  ),
  q12 as (
    insert into sch.order_positions (order_id,num,article_id,cnt,price)
      select q11.id, v.*
        from q11
          cross join (values (1,3,1,120),(2,1,2,10),(3,4,7,28)) v
      returning *
  )
  select 1

  with
    customers (_rid, name, address_id, note, orders) as (
      select row_number() over(), v.name, v.address_id, v.note, v.orders::jsonb
        from (values
          ('DimaO', 1, 'DimaO note',
            '[{"day":"2019-04-05","num":"040501","seller_id":5,"state":"booked"
              , "pos":
                [ {"num":1,"article_id":1,"cnt":5,"price":123}
                , {"num":2,"article_id":2,"cnt":8,"price":11} ]}
            , {"day":"2019-04-03","num":"040301","seller_id":3,"state":"paid"
              , "pos":
                [ {"num":1,"article_id":3,"cnt":5,"price":123}
                , {"num":2,"article_id":4,"cnt":2,"price":112}
                , {"num":3,"article_id":5,"cnt":1,"price":2000} ]}]'),
          ('Pushkin', 4, 'Pushkin note',
            '[{"day":"2019-04-04","num":"040401","seller_id":3,"state":"paid"
              , "pos":
                [ {"num":1,"article_id":3,"cnt":1,"price":1234}
                , {"num":2,"article_id":4,"cnt":3,"price":121} ]}
            , {"day":"2019-04-03","num":"040302","seller_id":3,"state":"delivered"
              , "pos":
                [ {"num":1,"article_id":3,"cnt":5,"price":123}
                , {"num":2,"article_id":4,"cnt":2,"price":112}
                , {"num":3,"article_id":2,"cnt":2,"price":800} ]}]')
          ) v(name, address_id, note, orders)
    ),
    i_customers (id,name) as (
      insert into sch.customers(name, address_id, note)
        select c.name, c.address_id, c.note from customers c
        returning id,name
    ),
    i_customers_res (_rid,id) as (
      select row_number() over(), id
        from i_customers
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
        
  -- based on https://stackoverflow.com/a/27996203/521370
