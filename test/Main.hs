{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.KDF.HMAC as KDF
import qualified Data.ByteString as BS
import qualified Data.Aeson as A
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Wycheproof as W

main :: IO ()
main = do
  wycheproof_sha256 <- TIO.readFile "etc/hkdf_sha256_test.json"
  wycheproof_sha512 <- TIO.readFile "etc/hkdf_sha512_test.json"
  let wycheproofs = do
        a <- A.decodeStrictText wycheproof_sha256 :: Maybe W.Wycheproof
        b <- A.decodeStrictText wycheproof_sha512 :: Maybe W.Wycheproof
        pure (a, b)
  case wycheproofs of
    Nothing -> error "couldn't parse wycheproof vectors"
    Just (w256, w512) -> defaultMain $ testGroup "ppad-hkdf" [
        wycheproof_tests SHA256 w256
      , wycheproof_tests SHA512 w512
      ]

data Hash = SHA256 | SHA512
  deriving Show

wycheproof_tests :: Hash -> W.Wycheproof -> TestTree
wycheproof_tests h W.Wycheproof {..} =
  testGroup ("wycheproof vectors (hkdf, " <> show h <> ")") $
    fmap (execute_group h) wp_testGroups

execute_group :: Hash -> W.HkdfTestGroup -> TestTree
execute_group h W.HkdfTestGroup {..} =
    testGroup msg (fmap (execute h) htg_tests)
  where
    msg = "keysize " <> show htg_keySize

execute :: Hash -> W.HkdfTest -> TestTree
execute h W.HkdfTest {..} = testCase t_msg $ do
    let ikm = ht_ikm
        sal = ht_salt
        inf = ht_info
        siz = ht_size
        pec = ht_okm
    if   ht_result == "invalid"
    then do
      out <- try (pure $! KDF.derive hmac sal inf siz ikm)
               :: IO (Either ErrorCall BS.ByteString)
      case out of
        Left _  -> assertBool "invalid" True
        Right o -> assertBool "invalid" (pec /= o)
    else do
      let out = KDF.derive hmac sal inf siz ikm
      assertEqual mempty pec out
  where
    hmac = case h of
      SHA256 -> SHA256.hmac
      SHA512 -> SHA512.hmac
    t_msg = "test " <> show ht_tcId -- XX embellish

