{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Signing(
    signBytes
) where

import           Data.ByteString (ByteString)

import           P

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
    
