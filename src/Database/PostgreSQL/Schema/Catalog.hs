{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Database.PostgreSQL.Schema.Catalog where

import Database.Schema.Def

data PgCatalog

------ tables ----------

-- attribute

instance CTabDef PgCatalog "pg_attribute" where
  type TTabDef PgCatalog "pg_attribute" =
    TabDefC
      '["oid","attrelid","attname","atttypid","attnum","attnotnull","atthasdef"]
      '["oid"] '[ '["attrelid","attname"], '["attrelid","attnum"]] False

  type TRelListFrom PgCatalog "pg_attribute" =
    '["attribute__class","attribute__type"]

  type TRelListTo PgCatalog "pg_attribute" = '[]

instance CTabDef PgCatalog tab => CFldDef PgCatalog tab "oid" where
  type TFldDef PgCatalog tab "oid" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_attribute" "attrelid" where
  type TFldDef PgCatalog "pg_attribute" "attrelid" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_attribute" "atttypid" where
  type TFldDef PgCatalog "pg_attribute" "atttypid" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_attribute" "attname" where
  type TFldDef PgCatalog "pg_attribute" "attname" = FldDefC "name" False False

instance CFldDef PgCatalog "pg_attribute" "attnum" where
  type TFldDef PgCatalog "pg_attribute" "attnum" = FldDefC "int2" False False

instance CFldDef PgCatalog "pg_attribute" "attnotnull" where
  type TFldDef PgCatalog "pg_attribute" "attnotnull" =
    FldDefC "bool" False False

instance CFldDef PgCatalog "pg_attribute" "atthasdef" where
  type TFldDef PgCatalog "pg_attribute" "atthasdef" = FldDefC "bool" False False

-- class

instance CTabDef PgCatalog "pg_class" where
  type TTabDef PgCatalog "pg_class" =
    TabDefC '["oid","relnamespace","relname","relkind"] '["oid"]
      '[ '["relnamespace","relname"]] False

  type TRelListFrom PgCatalog "pg_class" = '["class__namespace"]

  type TRelListTo PgCatalog "pg_class" =
    '["attribute__class", "constraint__class"]

instance CFldDef PgCatalog "pg_class" "relnamespace" where
  type TFldDef PgCatalog "pg_class" "relnamespace" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_class" "relname" where
  type TFldDef PgCatalog "pg_class" "relname" = FldDefC "name" False False

instance CFldDef PgCatalog "pg_class" "relkind" where
  type TFldDef PgCatalog "pg_class" "relkind" = FldDefC "char" False False
-- relkind	char:
-- r = ordinary table, i = index, S = sequence, v = view, m = materialized view
-- , c = composite type, t = TOAST table, f = foreign table

-- constraint

instance CTabDef PgCatalog "pg_constraint" where
  type TTabDef PgCatalog "pg_constraint" =
    TabDefC
      '["oid","connamespace","conname","contype","conrelid","confrelid"
      ,"conkey","confkey","confupdtype","confdeltype"]
      '["oid"] '[ '["connamespace","conname"]] False

  type TRelListFrom PgCatalog "pg_constraint" =
    '["constraint__class", "constraint__fclass"]

  type TRelListTo PgCatalog "pg_constraint" = '[]

instance CFldDef PgCatalog "pg_constraint" "connamespace" where
  type TFldDef PgCatalog "pg_constraint" "connamespace" =
    FldDefC "oid" False False

instance CFldDef PgCatalog "pg_constraint" "conname" where
  type TFldDef PgCatalog "pg_constraint" "conname" = FldDefC "name" False False

instance CFldDef PgCatalog "pg_constraint" "contype" where
  type TFldDef PgCatalog "pg_constraint" "contype" = FldDefC "char" False False
  -- c = check constraint, f = foreign key constraint
  -- , p = primary key constraint, u = unique constraint
  -- , t = constraint trigger, x = exclusion constrain

instance CFldDef PgCatalog "pg_constraint" "conrelid" where
  type TFldDef PgCatalog "pg_constraint" "conrelid" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_constraint" "confrelid" where
  type TFldDef PgCatalog "pg_constraint" "confrelid" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_constraint" "confupdtypeid" where
  type TFldDef PgCatalog "pg_constraint" "confupdtypeid" =
    FldDefC "bool" False False

instance CFldDef PgCatalog "pg_constraint" "confdeltypeid" where
  type TFldDef PgCatalog "pg_constraint" "confdeltypeid" =
    FldDefC "bool" False False

instance CFldDef PgCatalog "pg_constraint" "conkey" where
  type TFldDef PgCatalog "pg_constraint" "conkey" =
    FldDefC "int2[]" False False

instance CFldDef PgCatalog "pg_constraint" "confkey" where
  type TFldDef PgCatalog "pg_constraint" "confkey" =
    FldDefC "int2[]" False False

-- enum

instance CTabDef PgCatalog "pg_enum" where
  type TTabDef PgCatalog "pg_enum" =
    TabDefC '["oid","enumtypid","enumlabel","enumsortorder"] '["oid"]
      '[ '["enumtypid","enumlabel"], '["enumtypid","enumsortorder"]] False

  type TRelListFrom PgCatalog "pg_enum" = '["enum__type"]

  type TRelListTo PgCatalog "pg_enum" = '[]

instance CFldDef PgCatalog "pg_enum" "enumtypid" where
  type TFldDef PgCatalog "pg_enum" "enumtypid" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_enum" "enumlabel" where
  type TFldDef PgCatalog "pg_enum" "enumlabel" = FldDefC "name" False False

instance CFldDef PgCatalog "pg_enum" "enumsortorder" where
  type TFldDef PgCatalog "pg_enum" "enumsortorder" =
    FldDefC "float4" False False

-- namespace

instance CTabDef PgCatalog "pg_namespace" where
  type TTabDef PgCatalog "pg_namespace" =
    TabDefC '["oid","nspname"] '["oid"] '[ '["nspname"]] False

  type TRelListFrom PgCatalog "pg_namespace" = '[]

  type TRelListTo PgCatalog "pg_namespace" =
    '["class__namespace","constraint__namespace","type__namespace"]

instance CFldDef PgCatalog "pg_namespace" "nspname" where
  type TFldDef PgCatalog "pg_namespace" "nspname" = FldDefC "name" False False

-- type

instance CTabDef PgCatalog "pg_type" where
  type TTabDef PgCatalog "pg_type" =
    TabDefC '["oid","typnamespace","typname","typcategory","typelem"]
      '["oid"] '[ '["typnamespace","typname"]] False
  -- typcategory Codes
  -- A	- Array types
  -- B	- Boolean types
  -- C	- Composite types
  -- D	- Date/time types
  -- E	- Enum types
  -- G	- Geometric types
  -- I	- Network address types
  -- N	- Numeric types
  -- P	- Pseudo-types
  -- R	- Range types
  -- S	- String types
  -- T	- Timespan types
  -- U	- User-defined types
  -- V	- Bit-string types
  -- X	- unknown type

  -- in case of Array `typelem` is a type of element

  type TRelListFrom PgCatalog "pg_type" = '["type__namespace"]

  type TRelListTo PgCatalog "pg_type" =
    '["attribute__type", "constraint__type"]

instance CFldDef PgCatalog "pg_type" "typnamespace" where
  type TFldDef PgCatalog "pg_type" "typnamespace" = FldDefC "oid" False False

instance CFldDef PgCatalog "pg_type" "typname" where
  type TFldDef PgCatalog "pg_type" "typname" = FldDefC "name" False False

instance CFldDef PgCatalog "pg_type" "typcategory" where
  type TFldDef PgCatalog "pg_type" "typcategory" = FldDefC "char" False False

instance CFldDef PgCatalog "pg_type" "typelem" where
  type TFldDef PgCatalog "pg_type" "typelem" = FldDefC "oid" False False

---------- relations --------

instance CRelDef PgCatalog "attribute__class" where
  type TRelDef PgCatalog "attribute__class" =
    RelDefC "attribute" "class" '[ '("attrelid","oid")] DcRestrict

instance CRelDef PgCatalog "attribute__type" where
  type TRelDef PgCatalog "attribute__type" =
    RelDefC "attribute" "type" '[ '("atttypid","oid")] DcRestrict

instance CRelDef PgCatalog "class__namespace" where
  type TRelDef PgCatalog "class__namespace" =
    RelDefC "class" "namespace" '[ '("relnamespace","oid")] DcRestrict

instance CRelDef PgCatalog "constraint__class" where
  type TRelDef PgCatalog "constraint__class" =
    RelDefC "constraint" "class" '[ '("conrelid","oid")] DcRestrict

instance CRelDef PgCatalog "constraint__fclass" where
  type TRelDef PgCatalog "constraint__fclass" =
    RelDefC "constraint" "class" '[ '("confrelid","oid")] DcRestrict

instance CRelDef PgCatalog "constraint__namespace" where
  type TRelDef PgCatalog "constraint__namespace" =
    RelDefC "constraint" "namespace" '[ '("connamespace","oid")] DcRestrict

instance CRelDef PgCatalog "enum__type" where
  type TRelDef PgCatalog "enum__type" =
    RelDefC "enum" "type" '[ '("enumtypid","oid")] DcRestrict
--
instance CRelDef PgCatalog "type__namespace" where
  type TRelDef PgCatalog "type__namespace" =
    RelDefC "type" "namespace" '[ '("typnamespace","oid")] DcRestrict

----------- schema ----------

instance CSchema PgCatalog where
  type TSchema PgCatalog = "pg_catalog"

  type TTables PgCatalog =
    '[ "pg_attribute"
    , "pg_class"
    , "pg_constraint"
    , "pg_enum"
    , "pg_namespace"
    , "pg_type"]

  type TRels PgCatalog =
    '["attribute__class"
    , "attribute__type"
    , "class__namespace"
    , "constraint__class"
    , "constraint__fclass"
    , "constraint__namespace"
    , "enum__type"
    , "type__namespace"
    ]
