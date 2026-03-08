{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HListDoctest (runTests) where

import Control.Monad
import Data.Aeson (decode, encode, object, (.=))
import Data.Aeson.Key (fromString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 (pack)
import PgSchema.HList


type SimpleRec = HList '[ '( '("b", 0), Bool), '( '("a", 0), Int) ]

type InnerRec  = HList '[ '( '("x", 0), Int),  '( '("y", 0), Bool) ]
type OuterRec  = HList
  '[ '( '("outer", 0), Bool)
   , '( '("outer", 1), InnerRec)
   ]

runTests :: IO ()
runTests = do
  -- JSON round-trip (doctest $json)
  let val = False :* 1 :* HNil :: SimpleRec
  let enc = encode val
  unless (enc == pack "{\"b\":false,\"a\":1}")
    $ error $ "encode: got " ++ show enc

  case decode (pack "{\"a\":10,\"b\":true}") of
    Nothing -> error "decode: expected Just"
    Just (v :: SimpleRec) ->
      unless (v == (True :* 10 :* HNil))
        $ error $ "decode: got " ++ show v

  case decode (pack "{\"a\":10}") of
    Just (_ :: SimpleRec) -> error "decode: expected Nothing for missing key"
    Nothing -> pure ()

  -- Extra keys allowed
  case decode (pack "{\"a\":1,\"b\":false,\"extra\":42}") of
    Nothing -> error "decode with extra key: expected Just"
    Just (v' :: SimpleRec) ->
      unless (v' == val) $ error $ "decode with extra: got " ++ show v'

  -- Streaming (doctest $streaming)
  let bs = encode (object [fromString "a" .= (1 :: Int), fromString "b" .= False])
  case streamDecodeHList @'[ '( '("b", 0), Bool), '( '("a", 0), Int) ] bs of
    Left e -> error $ "streamDecodeHList: " ++ e
    Right h ->
      unless (h == val) $ error $ "streamDecodeHList: got " ++ show h

  case streamDecodeHList' @'[ '( '("b", 0), Bool), '( '("a", 0), Int) ] bs of
    Left e -> error $ "streamDecodeHList': " ++ e
    Right (h', rest) -> do
      unless (h' == val) $ error $ "streamDecodeHList': got " ++ show h'
      unless (BL.null rest) $ error "streamDecodeHList': expected null rest"

  let innerVal = (42 :: Int) :* True :* HNil :: InnerRec
  let outerVal = False :* innerVal :* HNil :: OuterRec
  let outerJson = pack "{\"outer___1\":{\"y\":true,\"x\":42},\"outer\":false}"

  case decode outerJson of
    Nothing -> error "nested decode: expected Just"
    Just (r :: OuterRec) ->
      unless (r == False :* (42 :* True :* HNil) :* HNil)
        $ error $ "nested decode: got " ++ show r

  case streamDecodeHList @'[ '( '("outer", 0), Bool), '( '("outer", 1), InnerRec) ] outerJson of
    Left e  -> error $ "nested streamDecodeHList: " ++ e
    Right r ->
      unless (r == outerVal)
        $ error $ "nested streamDecodeHList: got " ++ show r

  case streamDecodeHList' @'[ '( '("outer", 0), Bool), '( '("outer", 1), InnerRec) ] outerJson of
    Left e -> error $ "nested streamDecodeHList': " ++ e
    Right (r', rest) -> do
      unless (r' == outerVal)
        $ error $ "nested streamDecodeHList': got " ++ show r'
      unless (BL.null rest)
        $ error "nested streamDecodeHList': expected null rest"

  putStrLn "HList doctest-style checks passed."
