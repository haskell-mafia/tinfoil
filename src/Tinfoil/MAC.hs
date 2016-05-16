{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.MAC(
    hmacSHA256
) where

import           Crypto.Hash.Algorithms (SHA256)
import qualified Crypto.MAC.HMAC as Cryptonite

import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Tinfoil.Data.MAC

hmacSHA256 :: SigningKey -> ByteString -> MAC
hmacSHA256 (SigningKey k) msg =
  let mac = Cryptonite.hmac k msg :: Cryptonite.HMAC SHA256 in
  MAC . BS.pack . BA.unpack $ Cryptonite.hmacGetDigest mac
