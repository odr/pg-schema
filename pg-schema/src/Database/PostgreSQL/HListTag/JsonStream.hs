{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.HListTag.JsonStream where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import Data.Aeson.Decoding.Tokens (Tokens (..), TkRecord (..), TkArray (..), Lit (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as BL
import Database.PostgreSQL.HListTag.Type
import Database.PostgreSQL.PgTagged
import Database.Types.SchList (SchList (..))

--------------------------------------------------------------------------------
-- 2.4. Streaming decoding from JSON tokens
--------------------------------------------------------------------------------

-- $streaming
--
-- Stream decoding avoids building an intermediate 'Data.Aeson.Types.Object':
--
-- >>> let bs = encode (object ["b" .= False, "a" .= (1::Int)])
-- >>> streamDecodeHListTag @('[ '( '("b", 0), Bool), '( '("a", 0), Int) ]) bs
-- Right (b =: False) :* (a =: 1) :* HNil
--
-- The primed variant also returns the leftover input:
--
-- >>> let (Right (h, rest)) = streamDecodeHListTag' @('[ '( '("a", 0), Int), '( '("b", 0), Bool) ]) bs
-- >>> BL.null rest
-- True
--
-- | Generic initialization of an 'HListTag' lifted into an 'Alternative'
-- functor @f@ (e.g. @Maybe@). All slots are set to 'empty'.
class HEmpty ts f where
  hEmpty :: HListTag (Lifted ts f)

instance Alternative f => HEmpty '[] f where
  hEmpty = HNil

instance (Alternative f, HEmpty ts f) => HEmpty ('(s, t) ': ts) f where
  hEmpty = PgTag empty :* hEmpty @ts @f

-- | Rank-2 \"sequence\" for 'HListTag': collapse an extra 'Applicative'
-- layer around all fields.
class HUnLift ts f where
  hUnLift :: Applicative f => HListTag (Lifted ts f) -> f (HListTag ts)

instance Applicative f => HUnLift '[] f where
  hUnLift HNil = pure HNil

instance (Applicative f, HUnLift ts f) => HUnLift ('(s, t) ': ts) f where
  hUnLift (PgTag ft :* rest) =
    (:*) . PgTag <$> ft <*> hUnLift rest

-- | Update a lifted 'HListTag' by JSON key: for the matching field name,
-- parse the given 'Value' into the field's type and store it in @f@.
class HUpdateByKey ts f where
  hUpdateByKey
    :: Applicative f
    => Key.Key
    -> Value
    -> HListTag (Lifted ts f)
    -> Either String (HListTag (Lifted ts f))

instance Applicative f => HUpdateByKey '[] f where
  hUpdateByKey _ _ HNil = Right HNil

instance
  ( KnownSymNat sn s n
  , FromJSON t
  , Applicative f
  , HUpdateByKey ts f
  ) => HUpdateByKey ('(sn, t) ': ts) f where
  hUpdateByKey key v (PgTag ft :* rest)
    | key == expectedKey = do
        x <- firstPrefix "HListTag field parse error: " (parseEither parseJSON v)
        pure (PgTag (pure x) :* rest)
    | otherwise = do
        rest' <- hUpdateByKey @ts @f key v rest
        pure (PgTag ft :* rest')
    where
      expectedKey = Key.fromString (nameSymNat sn)

      firstPrefix :: String -> Either String a -> Either String a
      firstPrefix _ (Right a) = Right a
      firstPrefix pfx (Left e) = Left (pfx <> e)

-- | Parse a value of type @t@ from a JSON token stream (one value).
-- Returns the value and the continuation (rest of the stream after the value).
-- Used for streaming decode of field values without building an intermediate 'Value'.
class ParseFromTokens t where
  parseFromTokens :: Tokens k String -> Either String (t, k)

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (HListTag ts) where
  parseFromTokens (TkRecordOpen rec) = parseFromRecord @ts rec
  parseFromTokens _ = Left "HListTag: expected JSON object"

parseArray
  :: forall ts k.
     (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => TkArray k String
  -> Either String (SchList (HListTag ts), k)
parseArray = go []
  where
    go _ (TkArrayErr e) = Left e
    go acc (TkArrayEnd k) = Right (SchList (reverse acc), k)
    go acc (TkItem toks) = do
      (h, arr') <- parseFromTokens @(HListTag ts) toks
      go (h : acc) arr'

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (SchList (HListTag ts)) where
  parseFromTokens (TkArrayOpen arr) = parseArray @ts arr
  parseFromTokens _ = Left "SchList HListTag: expected JSON array"

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (Maybe (HListTag ts)) where
  parseFromTokens (TkLit LitNull k) = Right (Nothing, k)
  parseFromTokens (TkRecordOpen rec) = do
    (h, k) <- parseFromRecord @ts rec
    pure (Just h, k)
  parseFromTokens _ = Left "Maybe HListTag: expected null or JSON object"

instance {-# OVERLAPPABLE #-} FromJSON t => ParseFromTokens t where
  parseFromTokens toks = do
    (v, k) <- toEitherValue toks
    x <- firstPrefix "ParseFromTokens: " (parseEither parseJSON v)
    pure (x, k)
   where
    firstPrefix _ (Right a) = Right a
    firstPrefix pfx (Left e) = Left (pfx <> e)

-- | Update a lifted 'HListTag' by JSON key using the token stream of the value.
-- When the key matches the current field, parses from tokens (streaming for
-- HListTag/SchList/Maybe); when it does not, skips the value and recurses.
class HUpdateByKeyStream ts f where
  hUpdateByKeyStream
    :: Applicative f
    => Key.Key
    -> Tokens (TkRecord k String) String
    -> HListTag (Lifted ts f)
    -> Either String (HListTag (Lifted ts f), TkRecord k String)

instance Applicative f => HUpdateByKeyStream '[] f where
  hUpdateByKeyStream _key toks HNil = do
    (_, rec') <- toEitherValue toks
    pure (HNil, rec')

instance
  ( KnownSymNat sn s n
  , ParseFromTokens t
  , Applicative f
  , HUpdateByKeyStream ts f
  ) => HUpdateByKeyStream ('(sn, t) ': ts) f where
  hUpdateByKeyStream key toks (PgTag ft :* rest)
    | key == expectedKey = do
        (x, rec') <- parseFromTokens @t toks
        pure (PgTag (pure x) :* rest, rec')
    | otherwise = do
        (rest', rec'') <- hUpdateByKeyStream @ts @f key toks rest
        pure (PgTag ft :* rest', rec'')
   where
    expectedKey = Key.fromString (nameSymNat sn)

-- | Internal: parse an 'HListTag ts' from a JSON object token stream.
-- The result also returns the leftover continuation @k@ after the object.
parseFromRecord
  :: forall ts k. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => TkRecord k String
  -> Either String (HListTag ts, k)
parseFromRecord = go (hEmpty @ts @Maybe)
  where
    go
      :: HListTag (Lifted ts Maybe)
      -> TkRecord k String
      -> Either String (HListTag ts, k)
    go acc rec = case rec of
      TkRecordErr e -> Left e
      TkRecordEnd k ->
        case hUnLift @ts @Maybe acc of
          Nothing -> Left "HListTag: missing required field(s)"
          Just h  -> Right (h, k)
      TkPair key toks -> do
        (acc', rec') <- hUpdateByKeyStream @ts @Maybe key toks acc
        go acc' rec'

-- | Streamingly decode an 'HListTag' from a lazy 'ByteString', returning
-- both the decoded value and the leftover input.
streamDecodeHListTag'
  :: forall ts. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => BL.ByteString
  -> Either String (HListTag ts, BL.ByteString)
streamDecodeHListTag' bs =
  case lbsToTokens bs of
    TkRecordOpen rec ->
      case parseFromRecord @ts rec of
        Left e          -> Left e
        Right (h, rest) -> Right (h, rest)
    TkErr e -> Left e
    _       -> Left "HListTag: expected top-level JSON object"

-- | Streamingly decode an 'HListTag' from a lazy 'ByteString'.
streamDecodeHListTag
  :: forall ts. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => BL.ByteString
  -> Either String (HListTag ts)
streamDecodeHListTag bs = fmap fst (streamDecodeHListTag' @ts bs)
