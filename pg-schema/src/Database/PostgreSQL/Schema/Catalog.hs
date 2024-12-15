{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Database.PostgreSQL.Schema.Catalog where

import Data.Text as T
import Database.Schema.Def

data PgCatalog

type PGC name = "pg_catalog" ->> name

pgc :: Text -> NameNS
pgc = ("pg_catalog" ->>)

------ tables ----------

-- attribute

instance CTabDef PgCatalog (PGC "pg_attribute") where
  type TTabDef PgCatalog (PGC "pg_attribute") =
    'TabDef
      '["oid","attrelid","attname","atttypid","attnum","attnotnull","atthasdef"]
      '["oid"] '[ '["attrelid","attname"], '["attrelid","attnum"]]

instance CTabDef PgCatalog tab => CFldDef PgCatalog (tab :: NameNSK) "oid" where
  type TFldDef PgCatalog tab "oid" = 'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_attribute") "attrelid" where
  type TFldDef PgCatalog (PGC "pg_attribute") "attrelid" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_attribute") "atttypid" where
  type TFldDef PgCatalog (PGC "pg_attribute") "atttypid" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_attribute") "attname" where
  type TFldDef PgCatalog (PGC "pg_attribute") "attname" =
    'FldDef (PGC "name") False False

instance CFldDef PgCatalog (PGC "pg_attribute") "attnum" where
  type TFldDef PgCatalog (PGC "pg_attribute") "attnum" =
    'FldDef (PGC "int2") False False

instance CFldDef PgCatalog (PGC "pg_attribute") "attnotnull" where
  type TFldDef PgCatalog (PGC "pg_attribute") "attnotnull" =
    'FldDef (PGC "bool") False False

instance CFldDef PgCatalog (PGC "pg_attribute") "atthasdef" where
  type TFldDef PgCatalog (PGC "pg_attribute") "atthasdef" =
    'FldDef (PGC "bool") False False

-- class

instance CTabDef PgCatalog (PGC "pg_class") where
  type TTabDef PgCatalog (PGC "pg_class") =
    'TabDef '["oid","relnamespace","relname","relkind"] '["oid"]
      '[ '["relnamespace","relname"]]

instance CFldDef PgCatalog (PGC "pg_class") "relnamespace" where
  type TFldDef PgCatalog (PGC "pg_class") "relnamespace" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_class") "relname" where
  type TFldDef PgCatalog (PGC "pg_class") "relname" =
    'FldDef (PGC "name") False False

instance CFldDef PgCatalog (PGC "pg_class") "relkind" where
  type TFldDef PgCatalog (PGC "pg_class") "relkind" =
    'FldDef (PGC "char") False False
-- relkind	char:
-- r = ordinary table, i = index, S = sequence, v = view, m = materialized view
-- , c = composite type, t = TOAST table, f = foreign table

-- constraint

instance CTabDef PgCatalog (PGC "pg_constraint") where
  type TTabDef PgCatalog (PGC "pg_constraint") =
    'TabDef
      '["oid","connamespace","conname","contype","conrelid","confrelid"
      ,"conkey","confkey","confupdtypeid","confdeltypeid"]
      '["oid"] '[ '["connamespace","conname"]]

instance CFldDef PgCatalog (PGC "pg_constraint") "connamespace" where
  type TFldDef PgCatalog (PGC "pg_constraint") "connamespace" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "conname" where
  type TFldDef PgCatalog (PGC "pg_constraint") "conname" =
    'FldDef (PGC "name") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "contype" where
  type TFldDef PgCatalog (PGC "pg_constraint") "contype" =
    'FldDef (PGC "char") False False
  -- c = check constraint, f = foreign key constraint
  -- , p = primary key constraint, u = unique constraint
  -- , t = constraint trigger, x = exclusion constrain

instance CFldDef PgCatalog (PGC "pg_constraint") "conrelid" where
  type TFldDef PgCatalog (PGC "pg_constraint") "conrelid" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "confrelid" where
  type TFldDef PgCatalog (PGC "pg_constraint") "confrelid" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "confupdtypeid" where
  type TFldDef PgCatalog (PGC "pg_constraint") "confupdtypeid" =
    'FldDef (PGC "bool") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "confdeltypeid" where
  type TFldDef PgCatalog (PGC "pg_constraint") "confdeltypeid" =
    'FldDef (PGC "bool") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "conkey" where
  type TFldDef PgCatalog (PGC "pg_constraint") "conkey" =
    'FldDef (PGC "int2[]") False False

instance CFldDef PgCatalog (PGC "pg_constraint") "confkey" where
  type TFldDef PgCatalog (PGC "pg_constraint") "confkey" =
    'FldDef (PGC "int2[]") False False

-- enum

instance CTabDef PgCatalog (PGC "pg_enum") where
  type TTabDef PgCatalog (PGC "pg_enum") =
    'TabDef '["oid","enumtypid","enumlabel","enumsortorder"] '["oid"]
      '[ '["enumtypid","enumlabel"], '["enumtypid","enumsortorder"]]

instance CFldDef PgCatalog (PGC "pg_enum") "enumtypid" where
  type TFldDef PgCatalog (PGC "pg_enum") "enumtypid" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_enum") "enumlabel" where
  type TFldDef PgCatalog (PGC "pg_enum") "enumlabel" =
    'FldDef (PGC "name") False False

instance CFldDef PgCatalog (PGC "pg_enum") "enumsortorder" where
  type TFldDef PgCatalog (PGC "pg_enum") "enumsortorder" =
    'FldDef (PGC "float4") False False

-- namespace

instance CTabDef PgCatalog (PGC "pg_namespace") where
  type TTabDef PgCatalog (PGC "pg_namespace") =
    'TabDef '["oid","nspname"] '["oid"] '[ '["nspname"]]

instance CFldDef PgCatalog (PGC "pg_namespace") "nspname" where
  type TFldDef PgCatalog (PGC "pg_namespace") "nspname" =
    'FldDef (PGC "name") False False

-- type

instance CTabDef PgCatalog (PGC "pg_type") where
  type TTabDef PgCatalog (PGC "pg_type") =
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

instance CFldDef PgCatalog (PGC "pg_type") "typnamespace" where
  type TFldDef PgCatalog (PGC "pg_type") "typnamespace" =
    'FldDef (PGC "oid") False False

instance CFldDef PgCatalog (PGC "pg_type") "typname" where
  type TFldDef PgCatalog (PGC "pg_type") "typname" =
    'FldDef (PGC "name") False False

instance CFldDef PgCatalog (PGC "pg_type") "typcategory" where
  type TFldDef PgCatalog (PGC "pg_type") "typcategory" =
    'FldDef (PGC "char") False False

instance CFldDef PgCatalog (PGC "pg_type") "typelem" where
  type TFldDef PgCatalog (PGC "pg_type") "typelem" =
    'FldDef (PGC "oid") False False

---------- relations --------

instance CRelDef PgCatalog (PGC "attribute__class") where
  type TRelDef PgCatalog (PGC "attribute__class") =
    'RelDef (PGC "pg_attribute") (PGC "pg_class") '[ '("attrelid","oid")]

instance CRelDef PgCatalog (PGC "attribute__type") where
  type TRelDef PgCatalog (PGC "attribute__type") =
    'RelDef (PGC "pg_attribute") (PGC "pg_type") '[ '("atttypid","oid")]

instance CRelDef PgCatalog (PGC "class__namespace") where
  type TRelDef PgCatalog (PGC "class__namespace") =
    'RelDef (PGC "pg_class") (PGC "pg_namespace") '[ '("relnamespace","oid")]

instance CRelDef PgCatalog (PGC "constraint__class") where
  type TRelDef PgCatalog (PGC "constraint__class") =
    'RelDef (PGC "pg_constraint") (PGC "pg_class") '[ '("conrelid","oid")]

instance CRelDef PgCatalog (PGC "constraint__fclass") where
  type TRelDef PgCatalog (PGC "constraint__fclass") =
    'RelDef (PGC "pg_constraint") (PGC "pg_class") '[ '("confrelid","oid")]

instance CRelDef PgCatalog (PGC "constraint__namespace") where
  type TRelDef PgCatalog (PGC "constraint__namespace") =
    'RelDef (PGC "pg_constraint") (PGC "pg_namespace") '[ '("connamespace","oid")]

instance CRelDef PgCatalog (PGC "enum__type") where
  type TRelDef PgCatalog (PGC "enum__type") =
    'RelDef (PGC "pg_enum") (PGC "pg_type") '[ '("enumtypid","oid")]
--
instance CRelDef PgCatalog (PGC "type__namespace") where
  type TRelDef PgCatalog (PGC "type__namespace") =
    'RelDef (PGC "pg_type") (PGC "pg_namespace") '[ '("typnamespace","oid")]

----------- CTabRels ---------
instance CTabRels PgCatalog (PGC "pg_attribute") where
  type TFrom PgCatalog (PGC "pg_attribute") =
    '[PGC "attribute__class", PGC "attribute__type"]
  type TTo PgCatalog (PGC "pg_attribute") = '[]

instance CTabRels PgCatalog (PGC "pg_class") where
  type TFrom PgCatalog (PGC "pg_class") = '[PGC "class__namespace"]
  type TTo PgCatalog (PGC "pg_class") = '[PGC "attribute__class"
    , PGC "constraint__class", PGC "constraint__fclass"]

instance CTabRels PgCatalog (PGC "pg_constraint") where
  type TFrom PgCatalog (PGC "pg_constraint") =
    '[PGC "constraint__class", PGC "constraint__fclass", PGC "constraint__namespace"]
  type TTo PgCatalog (PGC "pg_constraint") = '[]

instance CTabRels PgCatalog (PGC "pg_enum") where
  type TFrom PgCatalog (PGC "pg_enum") = '[PGC "enum__type"]
  type TTo PgCatalog (PGC "pg_enum") = '[]

instance CTabRels PgCatalog (PGC "pg_namespace") where
  type TFrom PgCatalog (PGC "pg_namespace") = '[]
  type TTo PgCatalog (PGC "pg_namespace") =
    '[PGC "type__namespace", PGC "class__namespace", PGC "constraint__namespace"]

instance CTabRels PgCatalog (PGC "pg_type") where
  type TFrom PgCatalog (PGC "pg_type") = '[PGC "type__namespace"]
  type TTo PgCatalog (PGC "pg_type") = '[PGC "enum__type", PGC "attribute__type"]


----------- schema ----------

instance CSchema PgCatalog where
  -- type TSchema PgCatalog = "pg_catalog"

  type TTabs PgCatalog =
    '[ PGC "pg_attribute", PGC "pg_class", PGC "pg_constraint", PGC "pg_enum"
    , PGC "pg_namespace", PGC "pg_type" ]

  type TTypes PgCatalog =
    '[PGC "oid",PGC "int2",PGC "int2[]",PGC "float4",PGC "bool",PGC "name"
    ,PGC "char"]

instance CTypDef PgCatalog (PGC "oid") where
  type TTypDef PgCatalog (PGC "oid") = 'TypDef "N" Nothing '[]

instance CTypDef PgCatalog (PGC "int2") where
  type TTypDef PgCatalog (PGC "int2") = 'TypDef "N" Nothing '[]

instance CTypDef PgCatalog (PGC "int2[]") where
  type TTypDef PgCatalog (PGC "int2[]") = 'TypDef "A" (Just (PGC "int2")) '[]

instance CTypDef PgCatalog (PGC "float4") where
  type TTypDef PgCatalog (PGC "float4") = 'TypDef "N" Nothing '[]

instance CTypDef PgCatalog (PGC "bool") where
  type TTypDef PgCatalog (PGC "bool") = 'TypDef "B" Nothing '[]

instance CTypDef PgCatalog (PGC "name") where
  type TTypDef PgCatalog (PGC "name") = 'TypDef "S" Nothing '[]

instance CTypDef PgCatalog (PGC "char") where
  type TTypDef PgCatalog (PGC "char") = 'TypDef "S" Nothing '[]
