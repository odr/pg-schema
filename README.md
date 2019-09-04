# pg-schema

Type provider from PostgreSQL. With some batteries.

The aid of this package is to make productive type-safe access to PostgreSQL database.
Look at [Tutorial](https://github.com/odr/pg-schema/tree/master/pg-schema-tutorial)

## Brief

In compile time system information is getting from DB: tables, keys (private, unique, foreign), types (including arrays and enumerations). All information is moved to Type Level. I.e. you can wrote any statically-checked functions. E.g. there is a static guarantees that all db-field could be convert to haskell-field.

There are two way of generation this info:
- file generation (recommended). It can be done in project's Setup.hs. In this case on each compilation db will checked and info will be regenerated if there are some changes (checking is very fast). If db is not available you will use file which was generated before
- TH (obsolete + convenient for demonstration)

For data selection you can create regular haskell records with field names corresponding db-names (like in aeson). Besides that you can use fields with name of FK-constraints (from or to root table). Some small instances are generated for data-record and then you can populate it with data _without writing sql_ at all. It allows to get json-like data tree in one request (really, it is one SELECT to DB). On each level of tree you can set safe conditions (where, order by, limit/offset) using small EDSL.

All of this with static guarantees.

DML (Insert/Update/Delete) should be done later.
