{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Hash(
    hashFunction
  , hashSHA256
) where

import           Crypto.Hash.Algorithms (SHA256)
import qualified Crypto.Hash as Cryptonite

import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Tinfoil.Data.Hash

hashFunction :: HashFunction -> ByteString -> Hash
hashFunction SHA256 = hashSHA256

hashSHA256 :: ByteString -> Hash
hashSHA256 bs =
  let dgst = Cryptonite.hash bs :: Cryptonite.Digest SHA256 in
  Hash . BS.pack $ BA.unpack dgst
