{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Crypto.KDF.HMAC
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A pure HKDF implementation, as specified by
-- [RFC5869](https://datatracker.ietf.org/doc/html/rfc5869).

module Crypto.KDF.HMAC (
    -- * HMAC-based KDF
    hkdf

    -- * HKDF Internals
  , extract
  , expand

    -- internal types
  , HMAC
  , HMACEnv
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Internal as BI
import Data.Word (Word64)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- NB following synonym really only exists to make haddocks more
--    readable

-- | A HMAC function, taking a key as the first argument and the input
--   value as the second, producing a MAC digest.
--
--   >>> import qualified Crypto.Hash.SHA256 as SHA256
--   >>> :t SHA256.hmac
--   SHA256.hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
type HMAC = BS.ByteString -> BS.ByteString -> BS.ByteString

-- HMAC function and its associated outlength
data HMACEnv = HMACEnv
                 !HMAC
  {-# UNPACK #-} !Int

extract
  :: HMACEnv
  -> BS.ByteString  -- ^ salt
  -> BS.ByteString  -- ^ input keying material
  -> BS.ByteString  -- ^ pseudorandom key
extract (HMACEnv hmac hashlen) salt@(BI.PS _ _ l) ikm
  | l == 0    = hmac (BS.replicate hashlen 0x00) ikm
  | otherwise = hmac salt ikm
{-# INLINE extract #-}

expand
  :: HMACEnv
  -> BS.ByteString  -- ^ optional context and application-specific info
  -> Word64         -- ^ bytelength of output keying material
  -> BS.ByteString  -- ^ pseudorandom key
  -> BS.ByteString  -- ^ output keying material
expand (HMACEnv hmac hashlen) info (fi -> len) prk
    | len > 255 * hashlen = error "ppad-hkdf (expand): invalid outlength"
    | otherwise = BS.take len (go (1 :: Int) mempty mempty)
  where
    n = ceiling ((fi len :: Double) / (fi hashlen :: Double)) :: Int
    go !j t !tl
      | j > fi n = BS.toStrict (BSB.toLazyByteString t)
      | otherwise =
          let nt = hmac prk (tl <> info <> BS.singleton (fi j))
          in  go (succ j) (t <> BSB.byteString nt) nt
{-# INLINE expand #-}

-- | HMAC-based key derivation function.
--
--   The /salt/ and /info/ arguments are optional to the KDF, and may
--   be simply passed as 'mempty'. An empty salt will be replaced by
--   /hashlen/ zero bytes.
--
--   >>> import qualified Crypto.Hash.SHA256 as SHA256
--   >>> hkdf SHA256.hmac "my public salt" mempty 64 "my secret input"
--   <64-byte output keying material>
hkdf
  :: HMAC          -- ^ HMAC function
  -> BS.ByteString -- ^ salt
  -> BS.ByteString -- ^ optional context and application-specific info
  -> Word64        -- ^ bytelength of output keying material (<= 255 * hashlen)
  -> BS.ByteString -- ^ input keying material
  -> BS.ByteString -- ^ output keying material
hkdf hmac salt info len = expand env info len . extract env salt where
  env = HMACEnv hmac (fi (BS.length (hmac mempty mempty)))

