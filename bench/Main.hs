{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Crypto.KDF.HMAC as KDF
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString as BS

main :: IO ()
main = defaultMain [
    suite
  ]

hmac_sha256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac_sha256 k b = case SHA256.hmac k b of
  SHA256.MAC mac -> mac

hmac_sha512 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac_sha512 k b = case SHA512.hmac k b of
  SHA512.MAC mac -> mac

suite :: Benchmark
suite =
  bgroup "ppad-hkdf" [
    bgroup "HKDF-SHA256" [
      bench "derive (outlen 32)" $
        nf (KDF.derive hmac_sha256 "muh salt" "muh info" 32) "muh secret"
    ]
  , bgroup "HKDF-SHA512" [
      bench "derive (outlen 32)" $
        nf (KDF.derive hmac_sha512 "muh salt" "muh info" 32) "muh secret"
    ]
  ]

