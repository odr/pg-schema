{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Database.PostgreSQL.PgProduct where

import Data.Aeson
import Data.Data (Typeable)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import Util.HListTag


data a :.. b = a :.. b deriving (Generic, Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Generically (a :.. b))
-- ^ ':.' but with all needed instances

infixr 1 :..

instance (FromJSON a, FromJSON b) => FromJSON (a :.. b) where
  parseJSON v = do
    a <- parseJSON v
    b <- parseJSON v
    pure $ a :.. b

instance (ToJSON a, ToJSON b) => ToJSON (a :.. b) where
  toJSON (a :.. b) = case (toJSON a, toJSON b) of
    (Object oa, Object ob) -> Object $ oa <> ob
    _ -> error
      "try to convert to JSON a pair (:..) but one element is not an object"

instance (ToRow a, ToRow b) => ToRow (a :.. b) where
  toRow (a :.. b) = toRow $ a :. b

instance (FromRow a, FromRow b) => FromRow (a :.. b) where
  fromRow = do
    (a :. b) <- fromRow
    pure $ a :.. b

instance (FromJSON (a :.. b), Typeable (a :.. b)) => FromField (a :.. b) where
  fromField = fromJSONField

instance ToJSON (a :.. b) => ToField (a :.. b) where toField = toJSONField

instance ( IsoHListTag r a
         , IsoHListTag r b
         , AppendHListTag (Fields r a) (Fields r b)
         , SplitHListTag (Fields r a) (Fields r b)
         ) => IsoHListTag r (a :.. b) where

  type Fields r (a :.. b) = Append (Fields r a) (Fields r b)

  toHListTag (a :.. b) =
    appendHListTag (toHListTag @r a) (toHListTag @r b)

  fromHListTag h =
    let (h1, h2) = splitHListTag @(Fields r a) @(Fields r b) h
    in fromHListTag @r h1 :.. fromHListTag @r h2

-- >>> toHListTag @RenamerId $ ("f1" =: (5::Int)) :.. ("f2" =: True) :.. ("f3" =: [(1::Int)..3])
-- f1 := 5 :* f2 := True :* f3 := [1,2,3] :* HNil
-- >>> fromHListTag @RenamerId (toHListTag @RenamerId (("f1" =: (5::Int)) :.. ("f2" =: True) :.. ("f3" =: [(1::Int)..3]))) :: "f1" := Int :.. "f2" := Bool :.. "f3" := [Int]
-- PgTagged (Tagged 5) :.. (PgTagged (Tagged True) :.. PgTagged (Tagged [1,2,3]))
