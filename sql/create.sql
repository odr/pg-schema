create schema if not exists sch;

create extension if not exists "uuid-ossp";

create table sch.countries
  ( id serial primary key
  , name text not null
  , code text
  );


create table sch.cities
  ( id serial primary key
  , country_id int
  , name text
  , constraint city_country foreign key (country_id) references sch.countries(id)
  );

create table sch.addresses
  ( id serial primary key
  , city_id int
  , street text
  , home text
  , app text
  , zipcode text
  , constraint address_city foreign key (city_id) references sch.cities(id)
  );

create table sch.customers
  ( id serial primary key
  , name text not null
  , address_id int
  , note text
  , created_at timestamp with time zone not null default now()
  , updated_at timestamp with time zone
  , constraint cust_addr foreign key (address_id) references sch.addresses(id)
  );

create table sch.companies
  ( id serial primary key
  , name text not null
  , address_id int
  , created_at timestamp with time zone not null default now()
  , updated_at timestamp with time zone
  , constraint comp_addr foreign key (address_id) references sch.addresses(id)
  );

create type sch.order_state as enum ('paid','booked','delivered');

create table sch.orders
  ( id serial primary key
  , day date not null
  , num text not null
  , customer_id int not null
  , seller_id int not null
  , trader_id int
  , state sch.order_state
  , created_at timestamp with time zone not null default now()
  , updated_at timestamp with time zone
  , constraint ord_cust foreign key (customer_id) references sch.customers(id)
  , constraint ord_seller foreign key (seller_id) references sch.companies(id)
  , constraint ord_trader foreign key (trader_id) references sch.companies(id)
  );

create table sch.articles
  ( id serial primary key
  , name text not null
  , code text
  , created_at timestamp with time zone not null default now()
  , updated_at timestamp with time zone
  , unique (name)
  );

create table sch.order_positions
  ( order_id int not null
  , num int not null
  , article_id int not null
  , cnt int not null
  , price numeric not null
  , primary key (order_id, num)
  , constraint opos_order foreign key (order_id) references sch.orders(id)
  , constraint opos_article foreign key (article_id) references sch.articles(id)
  , unique (order_id, article_id)
  );
