{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE UndecidableInstances #-}
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

-- class

instance CTabDef PgCatalog (PGC "pg_class") where
  type TTabDef PgCatalog (PGC "pg_class") =
    'TabDef '["oid","relnamespace","relname","relkind"] '["oid"]
      '[ '["relnamespace","relname"]]

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

-- enum

instance CTabDef PgCatalog (PGC "pg_enum") where
  type TTabDef PgCatalog (PGC "pg_enum") =
    'TabDef '["oid","enumtypid","enumlabel","enumsortorder"] '["oid"]
      '[ '["enumtypid","enumlabel"], '["enumtypid","enumsortorder"]]

-- namespace

instance CTabDef PgCatalog (PGC "pg_namespace") where
  type TTabDef PgCatalog (PGC "pg_namespace") =
    'TabDef '["oid","nspname"] '["oid"] '[ '["nspname"]]

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
  type TTypDef PgCatalog (PGC "oid") = SimpleType "N"

instance CTypDef PgCatalog (PGC "int2") where
  type TTypDef PgCatalog (PGC "int2") = SimpleType "N"

instance CTypDef PgCatalog (PGC "int2[]") where
  type TTypDef PgCatalog (PGC "int2[]") = 'TypDef "A" (Just (PGC "int2")) '[]

instance CTypDef PgCatalog (PGC "float4") where
  type TTypDef PgCatalog (PGC "float4") = SimpleType "N"

instance CTypDef PgCatalog (PGC "bool") where
  type TTypDef PgCatalog (PGC "bool") = SimpleType "B"

instance CTypDef PgCatalog (PGC "name") where
  type TTypDef PgCatalog (PGC "name") = SimpleType "S"

instance CTypDef PgCatalog (PGC "char") where
  type TTypDef PgCatalog (PGC "char") = SimpleType "S"

---------- CFieldInfo ----------

-- CFieldInfo: plain columns (oid shared by many tables)
instance (CTabDef PgCatalog tab) =>
  CFieldInfo PgCatalog (tab :: NameNSK) "oid" where
  type TFieldInfo PgCatalog tab "oid" = 'RFPlain ('FldDef (PGC "oid") False False)

instance CFieldInfo PgCatalog (PGC "pg_attribute") "attrelid" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "attrelid" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_attribute") "attname" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "attname" =
    'RFPlain ('FldDef (PGC "name") False False)
instance CFieldInfo PgCatalog (PGC "pg_attribute") "atttypid" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "atttypid" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_attribute") "attnum" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "attnum" =
    'RFPlain ('FldDef (PGC "int2") False False)
instance CFieldInfo PgCatalog (PGC "pg_attribute") "attnotnull" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "attnotnull" =
    'RFPlain ('FldDef (PGC "bool") False False)
instance CFieldInfo PgCatalog (PGC "pg_attribute") "atthasdef" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "atthasdef" =
    'RFPlain ('FldDef (PGC "bool") False False)

instance CFieldInfo PgCatalog (PGC "pg_class") "relnamespace" where
  type TFieldInfo PgCatalog (PGC "pg_class") "relnamespace" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_class") "relname" where
  type TFieldInfo PgCatalog (PGC "pg_class") "relname" =
    'RFPlain ('FldDef (PGC "name") False False)
instance CFieldInfo PgCatalog (PGC "pg_class") "relkind" where
  type TFieldInfo PgCatalog (PGC "pg_class") "relkind" =
    'RFPlain ('FldDef (PGC "char") False False)

instance CFieldInfo PgCatalog (PGC "pg_constraint") "connamespace" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "connamespace" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "conname" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "conname" =
    'RFPlain ('FldDef (PGC "name") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "contype" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "contype" =
    'RFPlain ('FldDef (PGC "char") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "conrelid" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "conrelid" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "confrelid" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "confrelid" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "confupdtypeid" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "confupdtypeid" =
    'RFPlain ('FldDef (PGC "bool") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "confdeltypeid" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "confdeltypeid" =
    'RFPlain ('FldDef (PGC "bool") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "conkey" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "conkey" =
    'RFPlain ('FldDef (PGC "int2[]") False False)
instance CFieldInfo PgCatalog (PGC "pg_constraint") "confkey" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "confkey" =
    'RFPlain ('FldDef (PGC "int2[]") False False)

instance CFieldInfo PgCatalog (PGC "pg_enum") "enumtypid" where
  type TFieldInfo PgCatalog (PGC "pg_enum") "enumtypid" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_enum") "enumlabel" where
  type TFieldInfo PgCatalog (PGC "pg_enum") "enumlabel" =
    'RFPlain ('FldDef (PGC "name") False False)
instance CFieldInfo PgCatalog (PGC "pg_enum") "enumsortorder" where
  type TFieldInfo PgCatalog (PGC "pg_enum") "enumsortorder" =
    'RFPlain ('FldDef (PGC "float4") False False)

instance CFieldInfo PgCatalog (PGC "pg_namespace") "nspname" where
  type TFieldInfo PgCatalog (PGC "pg_namespace") "nspname" =
    'RFPlain ('FldDef (PGC "name") False False)

instance CFieldInfo PgCatalog (PGC "pg_type") "typnamespace" where
  type TFieldInfo PgCatalog (PGC "pg_type") "typnamespace" =
    'RFPlain ('FldDef (PGC "oid") False False)
instance CFieldInfo PgCatalog (PGC "pg_type") "typname" where
  type TFieldInfo PgCatalog (PGC "pg_type") "typname" =
    'RFPlain ('FldDef (PGC "name") False False)
instance CFieldInfo PgCatalog (PGC "pg_type") "typcategory" where
  type TFieldInfo PgCatalog (PGC "pg_type") "typcategory" =
    'RFPlain ('FldDef (PGC "char") False False)
instance CFieldInfo PgCatalog (PGC "pg_type") "typelem" where
  type TFieldInfo PgCatalog (PGC "pg_type") "typelem" =
    'RFPlain ('FldDef (PGC "oid") False False)

-- Relation names (RFFromHere / RFToHere)
instance CFieldInfo PgCatalog (PGC "pg_attribute") "attribute__class" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "attribute__class" =
    'RFFromHere (PGC "pg_class")
      '[ 'Ref "attrelid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_attribute") "attribute__type" where
  type TFieldInfo PgCatalog (PGC "pg_attribute") "attribute__type" =
    'RFFromHere (PGC "pg_type")
      '[ 'Ref "atttypid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_class") "attribute__class" where
  type TFieldInfo PgCatalog (PGC "pg_class") "attribute__class" =
    'RFToHere (PGC "pg_attribute")
      '[ 'Ref "attrelid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_class") "class__namespace" where
  type TFieldInfo PgCatalog (PGC "pg_class") "class__namespace" =
    'RFFromHere (PGC "pg_namespace")
      '[ 'Ref "relnamespace" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_class") "constraint__class" where
  type TFieldInfo PgCatalog (PGC "pg_class") "constraint__class" =
    'RFToHere (PGC "pg_constraint")
      '[ 'Ref "conrelid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_class") "constraint__fclass" where
  type TFieldInfo PgCatalog (PGC "pg_class") "constraint__fclass" =
    'RFToHere (PGC "pg_constraint")
      '[ 'Ref "confrelid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]

instance CFieldInfo PgCatalog (PGC "pg_constraint") "constraint__class" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "constraint__class" =
    'RFFromHere (PGC "pg_class")
      '[ 'Ref "conrelid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_constraint") "constraint__fclass" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "constraint__fclass" =
    'RFFromHere (PGC "pg_class")
      '[ 'Ref "confrelid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_constraint") "constraint__namespace" where
  type TFieldInfo PgCatalog (PGC "pg_constraint") "constraint__namespace" =
    'RFFromHere (PGC "pg_namespace")
      '[ 'Ref "connamespace" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]

instance CFieldInfo PgCatalog (PGC "pg_enum") "enum__type" where
  type TFieldInfo PgCatalog (PGC "pg_enum") "enum__type" =
    'RFFromHere (PGC "pg_type")
      '[ 'Ref "enumtypid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_type") "enum__type" where
  type TFieldInfo PgCatalog (PGC "pg_type") "enum__type" =
    'RFToHere (PGC "pg_enum")
      '[ 'Ref "enumtypid" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]

instance CFieldInfo PgCatalog (PGC "pg_type") "type__namespace" where
  type TFieldInfo PgCatalog (PGC "pg_type") "type__namespace" =
    'RFFromHere (PGC "pg_namespace")
      '[ 'Ref "typnamespace" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_namespace") "type__namespace" where
  type TFieldInfo PgCatalog (PGC "pg_namespace") "type__namespace" =
    'RFToHere (PGC "pg_type")
      '[ 'Ref "typnamespace" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_namespace") "class__namespace" where
  type TFieldInfo PgCatalog (PGC "pg_namespace") "class__namespace" =
    'RFToHere (PGC "pg_class")
      '[ 'Ref "relnamespace" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
instance CFieldInfo PgCatalog (PGC "pg_namespace") "constraint__namespace" where
  type TFieldInfo PgCatalog (PGC "pg_namespace") "constraint__namespace" =
    'RFToHere (PGC "pg_constraint")
      '[ 'Ref "connamespace" ('FldDef (PGC "oid") False False)
           "oid" ('FldDef (PGC "oid") False False) ]
