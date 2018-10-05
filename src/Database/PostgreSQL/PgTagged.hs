{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.PgTagged where

import Data.Coerce
import Data.Singletons.Prelude
import Data.Tagged
import Database.PostgreSQL.Convert
import Database.PostgreSQL.Rec
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToRow as PG
import Database.Schema.Def
import Database.Schema.Util.ToStar


#if MIN_VERSION_base(4,11,0)
newtype PgTagged a b = PgTagged (Tagged a b) deriving
  ( Eq, Read, Show, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup)
#else
newtype PgTagged a b = PgTagged (Tagged a b) deriving
  ( Eq, Read, Show, Ord, Functor, Applicative, Monad, Foldable, Monoid )
#endif

pgTag :: b -> PgTagged a b
pgTag = coerce

unPgTag :: PgTagged a b -> b
unPgTag = coerce

rePgTag :: PgTagged a c -> PgTagged b c
rePgTag = coerce

-- Instances for Tagged records (as tuples)
-- I don't know a _good_ way to support references in Tagged records.
-- So I support only plain records

instance
  ( fd ~ TFldDef sch tab n, CanConvert sch (FdType fd) (FdNullable fd) r
  , ToStar n, ToStar tab, ToStar fd)
  => CRecDef sch tab (PgTagged (n :: Symbol) r) where
    type TRecFlds sch tab (PgTagged n r) =
      '[ 'FldRecDefC ('FldRecInfoC n n (TFldDef sch tab n)) r]
    type TRecTo   sch tab (PgTagged n r) = '[]
    type TRecFrom sch tab (PgTagged n r) = '[]

instance CRecDef sch tab (PgTagged n r) => CRecDef sch tab (PgTagged '[n] r)
  where
    type TRecFlds sch tab (PgTagged '[n] r) = TRecFlds sch tab (PgTagged n r)
    type TRecTo sch tab (PgTagged '[n] r) = '[]
    type TRecFrom sch tab (PgTagged '[n] r) = '[]

instance
  ( fd ~ TFldDef sch tab n1, CanConvert sch (FdType fd) (FdNullable fd) r1
  , ToStar n1, CRecDef sch tab (PgTagged (n2 ': ns) rs), ToStar fd )
  => CRecDef sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs)) where
#if MIN_VERSION_singletons(2,4,0)
    type TRecFlds sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs))
      =  TRecFlds sch tab (PgTagged n1 r1)
      ++ TRecFlds sch tab (PgTagged (n2 ': ns) rs)
#else
    type TRecFlds sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs))
      =  TRecFlds sch tab (PgTagged n1 r1)
      :++ TRecFlds sch tab (PgTagged (n2 ': ns) rs)
#endif

    type TRecTo sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs)) = '[]
    type TRecFrom sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs)) = '[]

instance FromRow (Only b) => FromRow (PgTagged (n::Symbol) b) where
  fromRow = coerce @(Only b) <$> fromRow

instance FromRow (Only b) => FromRow (PgTagged ('[n]::[Symbol]) b) where
  fromRow = coerce @(Only b) <$> fromRow

instance (FromRow (Only a), FromRow (PgTagged (n2 ': ns) as))
  => FromRow (PgTagged (n1 ': n2 ': ns) (a,as)) where
    fromRow =
      coerce @(Only a, PgTagged (n2 ': ns) as) <$> ((,) <$> fromRow <*> fromRow)

instance ToRow (Only b) => ToRow (PgTagged (n::Symbol) b) where
  toRow = toRow @(Only b) . coerce

instance ToRow (Only b) => ToRow (PgTagged ('[n]::[Symbol]) b) where
  toRow = toRow @(Only b) . coerce

instance (ToRow (Only a), ToRow (PgTagged (n2 ': ns) as))
  => ToRow (PgTagged (n1 ': n2 ': ns) (a,as)) where
    toRow
      = toRow @(Only a PG.:. PgTagged (n2 ': ns) as)
      . ((PG.:.) <$> fst <*> snd) . coerce
