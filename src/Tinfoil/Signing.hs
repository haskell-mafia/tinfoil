{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Signing(
    signBytes
  , verifyBytes
) where

import           Data.ByteString (ByteString)

import           P

import           System.IO (IO)

import           Tinfoil.Comparison
import           Tinfoil.Data.KDF
import           Tinfoil.Data.MAC
import           Tinfoil.Data.Signing
import           Tinfoil.MAC

keyedHashFunction :: KeyedHashFunction -> SigningKey -> ByteString -> MAC
keyedHashFunction HMAC_SHA256 = hmacSHA256

-- | Sign a ByteString with a key using an authenticated hash function
-- (MAC). Don't use this directly unless you know what you're doing.
signBytes :: KeyedHashFunction -> SigningKey -> ByteString -> Signature
signBytes khf sk bs =
  Signature $ keyedHashFunction khf sk bs

-- | Verify that a signature of a ByteString was generated using a secret key.
-- Don't use this directly unless you know what you're doing.
verifyBytes :: KeyedHashFunction -> SigningKey -> ByteString -> Signature -> IO Verified
verifyBytes khf sk bs sig =
  let sig' = signBytes khf sk bs in do
  r <- (signatureBytes sig) `safeEq` (signatureBytes sig')
  if r
    then pure Verified
    else pure NotVerified
