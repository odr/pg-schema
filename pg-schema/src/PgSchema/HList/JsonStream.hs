{-# LANGUAGE UndecidableInstances #-}
module PgSchema.HList.JsonStream
  (streamDecodeHList, streamDecodeHList')
  where

import Data.Aeson
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import Data.Aeson.Decoding.Tokens (Tokens (..), TkRecord (..), TkArray (..), Lit (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as BL
import Data.Text as T
import PgSchema.HList.Type
import PgSchema.HList.Utils
import PgSchema.Schema
import Prelude as P

--------------------------------------------------------------------------------
-- Streaming decoding from JSON tokens
--------------------------------------------------------------------------------

-- $streaming
--
-- Stream decoding avoids building an intermediate 'Data.Aeson.Types.Object':
--
-- >>> let bs = encode (object ["b" .= False, "a" .= (1::Int)])
-- >>> streamDecodeHList @('[ '( '("b", 0), Bool), '( '("a", 0), Int) ]) bs
-- Right (b =: False) :* (a =: 1) :* HNil
--
-- The primed variant also returns the leftover input:
--
-- >>> let (Right (h, rest)) = streamDecodeHList' @('[ '( '("a", 0), Int), '( '("b", 0), Bool) ]) bs
-- >>> BL.null rest
-- True

-- | Parse a value of type @t@ from a JSON token stream (one value).
-- Returns the value and the continuation (rest of the stream after the value).
-- Used for streaming decode of field values without building an intermediate 'Value'.
class ParseFromTokens t where
  parseFromTokens :: Tokens k String -> Either String (t, k)

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (HList ts) where
  parseFromTokens (TkRecordOpen rec) = parseFromRecord @ts rec
  parseFromTokens _ = Left "HList: expected JSON object"

parseArray
  :: forall ts k.
     (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => TkArray k String
  -> Either String ([HList ts], k)
parseArray = go []
  where
    go _ (TkArrayErr e) = Left e
    go acc (TkArrayEnd k) = Right (P.reverse acc, k)
    go acc (TkItem toks) = do
      (h, arr') <- parseFromTokens @(HList ts) toks
      go (h : acc) arr'

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens [HList ts] where
  parseFromTokens (TkArrayOpen arr) = parseArray @ts arr
  parseFromTokens _ = Left "[HList]: expected JSON array"

instance (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
      => ParseFromTokens (Maybe (HList ts)) where
  parseFromTokens (TkLit LitNull k) = Right (Nothing, k)
  parseFromTokens (TkRecordOpen rec) = do
    (h, k) <- parseFromRecord @ts rec
    pure (Just h, k)
  parseFromTokens _ = Left "Maybe HList: expected null or JSON object"

instance {-# OVERLAPPABLE #-} FromJSON t => ParseFromTokens t where
  parseFromTokens toks = do
    (v, k) <- toEitherValue toks
    x <- firstPrefix "ParseFromTokens: " (parseEither parseJSON v)
    pure (x, k)
   where
    firstPrefix _ (Right a) = Right a
    firstPrefix pfx (Left e) = Left (pfx <> e)

-- | Update a lifted 'HList' by JSON key using the token stream of the value.
-- When the key matches the current field, parses from tokens (streaming for
-- HList/SchList/Maybe); when it does not, skips the value and recurses.
class HUpdateByKeyStream ts f where
  hUpdateByKeyStream
    :: Applicative f
    => Key.Key
    -> Tokens (TkRecord k String) String
    -> HList (Lifted ts f)
    -> Either String (HList (Lifted ts f), TkRecord k String)

instance Applicative f => HUpdateByKeyStream '[] f where
  hUpdateByKeyStream _key toks HNil = do
    (_, rec') <- toEitherValue toks
    pure (HNil, rec')

instance
  ( KnownSymNat sn
  , ParseFromTokens t
  , Applicative f
  , HUpdateByKeyStream ts f
  ) => HUpdateByKeyStream ('(sn, t) ': ts) f where
  hUpdateByKeyStream key toks (ft :* rest)
    | key == expectedKey = do
        (x, rec') <- parseFromTokens @t toks
        pure (pure x :* rest, rec')
    | otherwise = do
        (rest', rec'') <- hUpdateByKeyStream @ts @f key toks rest
        pure (ft :* rest', rec'')
   where
    expectedKey = Key.fromString (T.unpack $ nameSymNat sn)

-- | Internal: parse an 'HList ts' from a JSON object token stream.
-- The result also returns the leftover continuation @k@ after the object.
parseFromRecord
  :: forall ts k. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => TkRecord k String
  -> Either String (HList ts, k)
parseFromRecord = go (hEmpty @ts @Maybe)
  where
    go
      :: HList (Lifted ts Maybe)
      -> TkRecord k String
      -> Either String (HList ts, k)
    go acc rec = case rec of
      TkRecordErr e -> Left e
      TkRecordEnd k ->
        case hUnLift @ts @Maybe acc of
          Nothing -> Left "HList: missing required field(s)"
          Just h  -> Right (h, k)
      TkPair key toks -> do
        (acc', rec') <- hUpdateByKeyStream @ts @Maybe key toks acc
        go acc' rec'

-- | Streamingly decode an 'HList' from a lazy 'ByteString', returning
-- both the decoded value and the leftover input.
streamDecodeHList'
  :: forall ts. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => BL.ByteString
  -> Either String (HList ts, BL.ByteString)
streamDecodeHList' bs =
  case lbsToTokens bs of
    TkRecordOpen rec ->
      case parseFromRecord @ts rec of
        Left e          -> Left e
        Right (h, rest) -> Right (h, rest)
    TkErr e -> Left e
    _       -> Left "HList: expected top-level JSON object"

-- | Streamingly decode an 'HList' from a lazy 'ByteString'.
streamDecodeHList
  :: forall ts. (HEmpty ts Maybe, HUpdateByKeyStream ts Maybe, HUnLift ts Maybe)
  => BL.ByteString
  -> Either String (HList ts)
streamDecodeHList bs = fmap fst (streamDecodeHList' @ts bs)
