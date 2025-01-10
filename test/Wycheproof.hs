{-# LANGUAGE OverloadedStrings #-}

module Wycheproof (
    Wycheproof(..)
  , HkdfTestGroup(..)
  , HkdfTest(..)
  ) where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

data Wycheproof = Wycheproof {
    wp_numberOfTests :: !Int
  , wp_testGroups :: ![HkdfTestGroup]
  } deriving Show

instance A.FromJSON Wycheproof where
  parseJSON = A.withObject "Wycheproof" $ \m -> Wycheproof
    <$> m .: "numberOfTests"
    <*> m .: "testGroups"

data HkdfTestGroup = HkdfTestGroup {
    htg_keySize :: !Int
  , htg_type    :: !T.Text
  , htg_tests   :: ![HkdfTest]
  } deriving Show

instance A.FromJSON HkdfTestGroup where
  parseJSON = A.withObject "HkdfTestGroup" $ \m -> HkdfTestGroup
    <$> m .: "keySize"
    <*> m .: "type"
    <*> m .: "tests"

data HkdfTest = HkdfTest {
    ht_tcId    :: !Int
  , ht_comment :: !T.Text
  , ht_ikm     :: !BS.ByteString
  , ht_salt    :: !BS.ByteString
  , ht_info    :: !BS.ByteString
  , ht_size    :: !Word64
  , ht_okm     :: !BS.ByteString
  , ht_result  :: !T.Text
  } deriving Show

decodehex :: T.Text -> BS.ByteString
decodehex = B16.decodeLenient . TE.encodeUtf8

instance A.FromJSON HkdfTest where
  parseJSON = A.withObject "HkdfTest" $ \m -> HkdfTest
    <$> m .: "tcId"
    <*> m .: "comment"
    <*> fmap decodehex (m .: "ikm")
    <*> fmap decodehex (m .: "salt")
    <*> fmap decodehex (m .: "info")
    <*> m .: "size"
    <*> fmap decodehex (m .: "okm")
    <*> m .: "result"


