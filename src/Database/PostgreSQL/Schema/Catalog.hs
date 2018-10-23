{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Database.PostgreSQL.Schema.Catalog where

import Database.Schema.Def

data PgCatalog

------ tables ----------

-- attribute

instance CTabDef PgCatalog "pg_attribute" where
  type TTabDef PgCatalog "pg_attribute" =
    'TabDef
      '["oid","attrelid","attname","atttypid","attnum","attnotnull","atthasdef"]
      '["oid"] '[ '["attrelid","attname"], '["attrelid","attnum"]]

instance CTabDef PgCatalog tab => CFldDef PgCatalog tab "oid" where
  type TFldDef PgCatalog tab "oid" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_attribute" "attrelid" where
  type TFldDef PgCatalog "pg_attribute" "attrelid" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_attribute" "atttypid" where
  type TFldDef PgCatalog "pg_attribute" "atttypid" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_attribute" "attname" where
  type TFldDef PgCatalog "pg_attribute" "attname" = 'FldDef "name" False False

instance CFldDef PgCatalog "pg_attribute" "attnum" where
  type TFldDef PgCatalog "pg_attribute" "attnum" = 'FldDef "int2" False False

instance CFldDef PgCatalog "pg_attribute" "attnotnull" where
  type TFldDef PgCatalog "pg_attribute" "attnotnull" =
    'FldDef "bool" False False

instance CFldDef PgCatalog "pg_attribute" "atthasdef" where
  type TFldDef PgCatalog "pg_attribute" "atthasdef" = 'FldDef "bool" False False

-- class

instance CTabDef PgCatalog "pg_class" where
  type TTabDef PgCatalog "pg_class" =
    'TabDef '["oid","relnamespace","relname","relkind"] '["oid"]
      '[ '["relnamespace","relname"]]

instance CFldDef PgCatalog "pg_class" "relnamespace" where
  type TFldDef PgCatalog "pg_class" "relnamespace" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_class" "relname" where
  type TFldDef PgCatalog "pg_class" "relname" = 'FldDef "name" False False

instance CFldDef PgCatalog "pg_class" "relkind" where
  type TFldDef PgCatalog "pg_class" "relkind" = 'FldDef "char" False False
-- relkind	char:
-- r = ordinary table, i = index, S = sequence, v = view, m = materialized view
-- , c = composite type, t = TOAST table, f = foreign table

-- constraint

instance CTabDef PgCatalog "pg_constraint" where
  type TTabDef PgCatalog "pg_constraint" =
    'TabDef
      '["oid","connamespace","conname","contype","conrelid","confrelid"
      ,"conkey","confkey","confupdtypeid","confdeltypeid"]
      '["oid"] '[ '["connamespace","conname"]]

instance CFldDef PgCatalog "pg_constraint" "connamespace" where
  type TFldDef PgCatalog "pg_constraint" "connamespace" =
    'FldDef "oid" False False

instance CFldDef PgCatalog "pg_constraint" "conname" where
  type TFldDef PgCatalog "pg_constraint" "conname" = 'FldDef "name" False False

instance CFldDef PgCatalog "pg_constraint" "contype" where
  type TFldDef PgCatalog "pg_constraint" "contype" = 'FldDef "char" False False
  -- c = check constraint, f = foreign key constraint
  -- , p = primary key constraint, u = unique constraint
  -- , t = constraint trigger, x = exclusion constrain

instance CFldDef PgCatalog "pg_constraint" "conrelid" where
  type TFldDef PgCatalog "pg_constraint" "conrelid" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_constraint" "confrelid" where
  type TFldDef PgCatalog "pg_constraint" "confrelid" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_constraint" "confupdtypeid" where
  type TFldDef PgCatalog "pg_constraint" "confupdtypeid" =
    'FldDef "bool" False False

instance CFldDef PgCatalog "pg_constraint" "confdeltypeid" where
  type TFldDef PgCatalog "pg_constraint" "confdeltypeid" =
    'FldDef "bool" False False

instance CFldDef PgCatalog "pg_constraint" "conkey" where
  type TFldDef PgCatalog "pg_constraint" "conkey" =
    'FldDef "int2[]" False False

instance CFldDef PgCatalog "pg_constraint" "confkey" where
  type TFldDef PgCatalog "pg_constraint" "confkey" =
    'FldDef "int2[]" False False

-- enum

instance CTabDef PgCatalog "pg_enum" where
  type TTabDef PgCatalog "pg_enum" =
    'TabDef '["oid","enumtypid","enumlabel","enumsortorder"] '["oid"]
      '[ '["enumtypid","enumlabel"], '["enumtypid","enumsortorder"]]

instance CFldDef PgCatalog "pg_enum" "enumtypid" where
  type TFldDef PgCatalog "pg_enum" "enumtypid" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_enum" "enumlabel" where
  type TFldDef PgCatalog "pg_enum" "enumlabel" = 'FldDef "name" False False

instance CFldDef PgCatalog "pg_enum" "enumsortorder" where
  type TFldDef PgCatalog "pg_enum" "enumsortorder" =
    'FldDef "float4" False False

-- namespace

instance CTabDef PgCatalog "pg_namespace" where
  type TTabDef PgCatalog "pg_namespace" =
    'TabDef '["oid","nspname"] '["oid"] '[ '["nspname"]]

instance CFldDef PgCatalog "pg_namespace" "nspname" where
  type TFldDef PgCatalog "pg_namespace" "nspname" = 'FldDef "name" False False

-- type

instance CTabDef PgCatalog "pg_type" where
  type TTabDef PgCatalog "pg_type" =
    'TabDef '["oid","typnamespace","typname","typcategory","typelem"]
      '["oid"] '[ '["typnamespace","typname"]]
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

instance CFldDef PgCatalog "pg_type" "typnamespace" where
  type TFldDef PgCatalog "pg_type" "typnamespace" = 'FldDef "oid" False False

instance CFldDef PgCatalog "pg_type" "typname" where
  type TFldDef PgCatalog "pg_type" "typname" = 'FldDef "name" False False

instance CFldDef PgCatalog "pg_type" "typcategory" where
  type TFldDef PgCatalog "pg_type" "typcategory" = 'FldDef "char" False False

instance CFldDef PgCatalog "pg_type" "typelem" where
  type TFldDef PgCatalog "pg_type" "typelem" = 'FldDef "oid" False False

---------- relations --------

instance CRelDef PgCatalog "attribute__class" where
  type TRelDef PgCatalog "attribute__class" =
    'RelDef "pg_attribute" "pg_class" '[ '("attrelid","oid")]

instance CRelDef PgCatalog "attribute__type" where
  type TRelDef PgCatalog "attribute__type" =
    'RelDef "pg_attribute" "pg_type" '[ '("atttypid","oid")]

instance CRelDef PgCatalog "class__namespace" where
  type TRelDef PgCatalog "class__namespace" =
    'RelDef "pg_class" "pg_namespace" '[ '("relnamespace","oid")]

instance CRelDef PgCatalog "constraint__class" where
  type TRelDef PgCatalog "constraint__class" =
    'RelDef "pg_constraint" "pg_class" '[ '("conrelid","oid")]

instance CRelDef PgCatalog "constraint__fclass" where
  type TRelDef PgCatalog "constraint__fclass" =
    'RelDef "pg_constraint" "pg_class" '[ '("confrelid","oid")]

instance CRelDef PgCatalog "constraint__namespace" where
  type TRelDef PgCatalog "constraint__namespace" =
    'RelDef "pg_constraint" "pg_namespace" '[ '("connamespace","oid")]

instance CRelDef PgCatalog "enum__type" where
  type TRelDef PgCatalog "enum__type" =
    'RelDef "pg_enum" "pg_type" '[ '("enumtypid","oid")]
--
instance CRelDef PgCatalog "type__namespace" where
  type TRelDef PgCatalog "type__namespace" =
    'RelDef "pg_type" "pg_namespace" '[ '("typnamespace","oid")]

----------- schema ----------

instance CSchema PgCatalog where
  type TSchema PgCatalog = "pg_catalog"

  type TTabs PgCatalog =
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

instance CTypDef PgCatalog "oid" where
  type TTypDef PgCatalog "oid" = 'TypDef "N" Nothing '[]

instance CTypDef PgCatalog "int2" where
  type TTypDef PgCatalog "int2" = 'TypDef "N" Nothing '[]

instance CTypDef PgCatalog "int2[]" where
  type TTypDef PgCatalog "int2[]" = 'TypDef "A" (Just "int2") '[]

instance CTypDef PgCatalog "float4" where
  type TTypDef PgCatalog "float4" = 'TypDef "N" Nothing '[]

instance CTypDef PgCatalog "bool" where
  type TTypDef PgCatalog "bool" = 'TypDef "B" Nothing '[]

instance CTypDef PgCatalog "name" where
  type TTypDef PgCatalog "name" = 'TypDef "S" Nothing '[]

instance CTypDef PgCatalog "char" where
  type TTypDef PgCatalog "char" = 'TypDef "S" Nothing '[]
