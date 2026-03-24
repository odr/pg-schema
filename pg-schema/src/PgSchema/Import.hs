{-| Shared import surface for /generated/ schema modules only.

Do @not@ import this module from handwritten application or library code.

@pg-schema@ tooling emits a schema module (per database or slice) that imports
"PgSchema.Import" to pull in the type classes, type families, and promoted data
that describe your schema at compile time. That generated module is what your
application should import alongside "PgSchema.DML" (and optionally
"PgSchema.Generation" for the codegen executable).

"PgSchema.Import" is a thin re-export hub: it exists so generated files stay short
and stable across @pg-schema@ versions, and so the generator does not need to
duplicate long import lists from internal modules. It is part of the package’s
@exposed-modules@ only so those generated modules (which live in your project, not
inside this package) can depend on it via ordinary package imports.

If you import "PgSchema.Import" directly, you bypass the intended layering, gain no
extra capability, and risk silent breakage when the re-export set changes.
Treat it as an implementation detail of codegen output, not as a public API to build on.
-}
module PgSchema.Import
  (
  -- * CSchema class
    CSchema(..)
  -- * CTabDef class
  , CTabDef(..), TabDef'(..)
  -- * CDBFieldInfo class
  , CDBFieldInfo(..), FldDef'(..), RelDef'(..)
  -- * CTypDef class
  , CTypDef(..), TypDef'(..)
  -- * RecField class
  , RecField'(..), RecFieldK, Ref'(..)
  -- * TRelDef type family
  , CRelDef(..), RdFrom, RdTo
  -- * CTabRels class
  , CTabRels(..)
  -- * PGEnum type
  , PGEnum
  -- * NameNS type classes
  , NameNSK, type (->>)
  , ToStar
  ) where

import PgSchema.Schema
import PgSchema.Types
import PgSchema.Utils.Internal
