{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Crypto.KDF.HMAC as K
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512

main :: IO ()
main = defaultMain [
    suite
  ]

suite :: Benchmark
suite =
  bgroup "ppad-hkdf" [
    bgroup "HKDF-SHA256" [
      bench "32" $ nf (K.hkdf SHA256.hmac "muh salt" "muh info" 32) "muh secret"
    ]
  , bgroup "HKDF-SHA512" [
      bench "32" $ nf (K.hkdf SHA512.hmac "muh salt" "muh info" 32) "muh secret"
    ]
  ]

