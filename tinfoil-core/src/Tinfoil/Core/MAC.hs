{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Core.MAC(
    hmacSHA256
  , macBytes
  , verifyMAC
) where

import           Crypto.Hash.Algorithms (SHA256)
import qualified Crypto.MAC.HMAC as Cryptonite

import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           System.IO (IO)

import           Tinfoil.Core.Comparison
import           Tinfoil.Core.Data.Key
import           Tinfoil.Core.Data.MAC
import           Tinfoil.Core.Data.Verify

hmacSHA256 :: SymmetricKey -> ByteString -> MAC
hmacSHA256 (SymmetricKey k) msg =
  let mac = Cryptonite.hmac k msg :: Cryptonite.HMAC SHA256 in
  MAC . BS.pack . BA.unpack $ Cryptonite.hmacGetDigest mac

keyedHashFunction :: KeyedHashFunction -> SymmetricKey -> ByteString -> MAC
keyedHashFunction HMAC_SHA256 = hmacSHA256

-- | Generate a message authentication code for a ByteString with a
-- key using an authenticated hash function (MAC). Don't use this
-- directly unless you know what you're doing.
macBytes :: KeyedHashFunction -> SymmetricKey -> ByteString -> MAC
macBytes khf sk bs =
  keyedHashFunction khf sk bs

-- | Verify that a MAC of a ByteString was generated using a secret key.
-- Don't use this directly unless you know what you're doing.
verifyMAC :: KeyedHashFunction -> SymmetricKey -> ByteString -> MAC -> IO Verified
verifyMAC khf sk bs sig =
  let sig' = macBytes khf sk bs in do
  r <- (unMAC sig) `safeEq` (unMAC sig')
  if r
    then pure Verified
    else pure NotVerified

