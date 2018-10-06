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
  ( ToStar n, ToStar tab, ToStar fd, fd ~ TFldDef sch tab n
  , CanConvert sch (FdType fd) (FdNullable fd) r )
  => CRecDef sch tab (PgTagged (n :: Symbol) r) where
    type TRecDef sch tab (PgTagged n r) =
      '[ 'FldRecDefC ('FldPlain ('FldRecInfoC n n (TFldDef sch tab n))) r]

instance CRecDef sch tab (PgTagged n r) => CRecDef sch tab (PgTagged '[n] r)
  where
    type TRecDef sch tab (PgTagged '[n] r) = TRecDef sch tab (PgTagged n r)

instance
  ( ToStar n1, CRecDef sch tab (PgTagged (n2 ': ns) rs), ToStar fd
  , fd ~ TFldDef sch tab n1, CanConvert sch (FdType fd) (FdNullable fd) r1 )
  => CRecDef sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs)) where
#if MIN_VERSION_singletons(2,4,0)
    type TRecDef sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs))
      =  TRecDef sch tab (PgTagged n1 r1)
      ++ TRecDef sch tab (PgTagged (n2 ': ns) rs)
#else
    type TRecDef sch tab (PgTagged (n1 ': n2 ': ns) (r1,rs))
      =  TRecDef sch tab (PgTagged n1 r1)
      :++ TRecDef sch tab (PgTagged (n2 ': ns) rs)
#endif

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
