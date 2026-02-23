{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Database.PostgreSQL.HListTag where

import Data.Aeson
import Data.Kind (Type)
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

-- | Type-level append for list of types.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Heterogeneous list with elements @'PgTagged' (s :: (Symbol, Nat)) t@.
data HListTag (xs :: [Type]) where
  HNil  :: HListTag '[]
  HCons :: PgTagged s t -> HListTag xs -> HListTag (PgTagged s t ': xs)

infixr 5 `HCons`

instance Show (HListTag '[]) where
  show HNil = "HNil"

instance (Show (PgTagged s t), Show (HListTag xs)) => Show (HListTag (PgTagged s t ': xs)) where
  show (HCons x xs) = "HCons (" ++ show x ++ ") (" ++ show xs ++ ")"

instance Semigroup (HListTag '[]) where
  HNil <> HNil = HNil

instance Monoid (HListTag '[]) where
  mempty = HNil

-- | Append two tagged lists. Result type is the type-level concatenation.
hAppend :: HListTag xs -> HListTag ys -> HListTag (xs ++ ys)
hAppend HNil ys = ys
hAppend (HCons x xs) ys = HCons x (hAppend xs ys)

instance FromJSON (HListTag '[]) where
  parseJSON = withObject "HListTag" $ \_ -> pure HNil

instance (FromJSON (PgTagged s t), FromJSON (HListTag xs))
  => FromJSON (HListTag (PgTagged s t ': xs)) where
  parseJSON v = HCons <$> parseJSON v <*> parseJSON v

instance ToJSON (HListTag '[]) where
  toJSON _ = object []

instance (ToJSON (PgTagged s t), ToJSON (HListTag xs))
  => ToJSON (HListTag (PgTagged s t ': xs)) where
  toJSON (HCons x xs) = case (toJSON x, toJSON xs) of
    (Object a, Object b) -> Object (a <> b)
    _ -> error "HListTag.ToJSON: PgTagged elements must produce JSON objects"

instance FromRow (HListTag '[]) where
  fromRow = pure HNil

instance (FromRow (PgTagged s t), FromRow (HListTag xs))
  => FromRow (HListTag (PgTagged s t ': xs)) where
  fromRow = HCons <$> fromRow <*> fromRow

instance ToRow (HListTag '[]) where
  toRow _ = []

instance (ToRow (PgTagged s t), ToRow (HListTag xs))
  => ToRow (HListTag (PgTagged s t ': xs)) where
  toRow (HCons x xs) = toRow x ++ toRow xs
