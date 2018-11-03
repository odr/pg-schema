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
    insert into sch.orders(day,num,customer_id,seller_id)
      select now(),'n1',q1.id,1
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
    insert into sch.orders(day,num,customer_id,seller_id)
      select now(),'n1',q1.id,1
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
    insert into sch.orders(day,num,customer_id,seller_id)
      select now(),'n2',q1.id,3
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
