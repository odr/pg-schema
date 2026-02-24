{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Database.PostgreSQL.PgTagged where

import Data.Aeson
import Data.Coerce
import Data.Kind (Constraint)
import Data.Maybe.Singletons
#ifdef MK_HASHABLE
import Data.Hashable
#endif
import Data.String
import Data.Tagged
import Data.Text as T
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.ToRow as PG
import Database.Schema.Def
import Database.Schema.Rec
import Database.Types.Aggr
import GHC.TypeLits as TL
import PgSchema.Util
import Prelude as P
import Prelude.Singletons
import Type.Reflection
import Database.Types.SchList (SchList)
#ifdef MK_ARBITRARY
import Test.QuickCheck
#endif
#ifdef MK_FLAT
import Flat as F
#endif


newtype PgTagged a b = PgTagged (Tagged a b)
  -- deriving stock (Read, Show)
  deriving newtype
  ( Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid
  , Semigroup, Num, Real, Integral, Enum, Bounded, RealFloat, RealFrac, Floating
  , Fractional, IsString )

instance (KnownSymbol s, Show b) => Show (PgTagged (s :: Symbol) b)  where
  show (PgTag v) = symbolVal (Proxy @s) <> " =: " <> P.show v

instance (KnownSymNat sn, Show b)
  => Show (PgTagged (sn :: SymNat) b)  where
    show (PgTag v) = T.unpack (nameSymNat sn) <> " =: " <> P.show v

#ifdef MK_ARBITRARY
instance Arbitrary b => Arbitrary (PgTagged a b) where
  arbitrary = pgTag <$> arbitrary
#endif

#ifdef MK_FLAT
instance Flat b => Flat (PgTagged a b) where
  encode = F.encode . unPgTag
  decode = pgTag <$> F.decode
  size = F.size . unPgTag

#endif

#ifdef MK_HASHABLE
instance Hashable b => Hashable (PgTagged a b) where
  hashWithSalt s = hashWithSalt @b s . coerce
#endif


{-# COMPLETE PgTag #-}
pattern PgTag :: forall a b. b -> PgTagged a b
pattern PgTag b = PgTagged (Tagged b)

(=:) :: forall b. forall a -> b -> PgTagged a b
(=:) _ = coerce
infixr 5 =:

type s := t = PgTagged s t

pgTag :: forall a b. b -> PgTagged a b
pgTag = coerce

unPgTag :: forall a b. PgTagged a b -> b
unPgTag = coerce

rePgTag :: forall a b c. PgTagged a c -> PgTagged b c
rePgTag = coerce

instance (ToStar a, FromJSON b) => FromJSON (PgTagged (a::Symbol) b) where
  parseJSON = withObject "PgTagged " \v ->
    pgTag <$> v .: fromString (T.unpack $ demote @a)

instance (ToStar a, ToJSON b) => ToJSON (PgTagged (a::Symbol) b) where
  toJSON v = object [fromString (T.unpack $ demote @a) .= unPgTag v]

-- | Tag as type-level pair (Symbol, Nat). JSON uses the Symbol as key.
instance (KnownSymbol s, FromJSON b) => FromJSON (PgTagged '(s, n) b) where
  parseJSON = withObject "PgTagged (Symbol, Nat)" \v ->
    pgTag <$> v .: fromString (T.unpack $ demote @s)

instance (KnownSymbol s, ToJSON b) => ToJSON (PgTagged '(s, n) b) where
  toJSON v = object [fromString (T.unpack $ demote @s) .= unPgTag v]

instance FromRow (Only b) => FromRow (PgTagged '(s, n) b) where
  fromRow = coerce @(Only b) <$> fromRow

instance ToRow (Only b) => ToRow (PgTagged '(s, n) b) where
  toRow = toRow @(Only b) . coerce

-- used in child (SchList) fields
instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) a) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) a) where
  toField = toJSONField

instance
  (FromJSON a, Typeable a, KnownSymbol n)
  => FromField (PgTagged (n::Symbol) (SchList a)) where
  fromField = fromJSONField

instance (ToJSON a, ToStar n) => ToField (PgTagged (n::Symbol) (SchList a)) where
  toField = toJSONField

-- | Type-level field description for a tagged field.
-- Aggregates are handled specially; all other cases delegate to 'TFieldInfo'.
type family PgTaggedFldDef sch t n r :: RecFieldK NameNSK where
  PgTaggedFldDef sch t n (Aggr' "count" r) =
    'RFAggr ('FldDef ('NameNS "pg_catalog" "int8") 'False 'False) "count" 'True
  PgTaggedFldDef sch t n (Aggr "count" r) = PgTaggedFldDef sch t n (Aggr' "count" r)
  PgTaggedFldDef sch t n (Aggr' fname r) = 'RFAggr
    (FromMaybe
      (TypeError (TL.Text "Can't process aggregate with name " :<>: TL.ShowType fname))
      (AggrFldDefJ' (TTypDefSym1 sch) fname ('Just (GetFldDef sch t n)))) fname 'False
  PgTaggedFldDef sch t n (Aggr fname r) = 'RFAggr
    (FromMaybe
      (TypeError (TL.Text "Can't process aggregate with name " :<>: TL.ShowType fname))
      (AggrFldDefJ (TTypDefSym1 sch) fname ('Just (GetFldDef sch t n)))) fname 'True
  -- Non-aggregate fields: use schema-level field info.
  PgTaggedFldDef sch t n r = TFieldInfo sch t n

-- | Extra context needed to use 'PgTaggedFldDef':
-- for aggregates we don't need schema info; for plain/rel fields we require 'CFieldInfo'.
type family PgTaggedFldCtx sch t n r :: Constraint where
  PgTaggedFldCtx sch t n (Aggr' fname r) = ()
  PgTaggedFldCtx sch t n (Aggr fname r)  = ()
  PgTaggedFldCtx sch t n r               = CFieldInfo sch t n

instance ( PgTaggedFldCtx sch t n r
         , MkRecField sch (
             FieldKind (Fst (Head (TRecordInfo sch t (PgTagged n r))))) r
         , ToStar n, ToStar t) =>
  CRecordInfo sch t (PgTagged (n::Symbol) r) where
  type TRecordInfo sch t (PgTagged n r) = '[ '( 'FieldInfo n n (PgTaggedFldDef sch t n r), r) ]
  getRecordInfo = RecordInfo (demote @t) [FieldInfo (demote @n) (demote @n)
    $ mkRecField @sch @(FieldKind (Fst (Head (TRecordInfo sch t (PgTagged n r))))) @r]

instance FromRow (Only b) => FromRow (PgTagged (n::Symbol) b) where
  fromRow = coerce @(Only b) <$> fromRow

instance ToRow (Only b) => ToRow (PgTagged (n::Symbol) b) where
  toRow = toRow @(Only b) . coerce
