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
