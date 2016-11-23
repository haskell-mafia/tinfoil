{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Core.Hash(
    hash
  , hashSHA256
) where

import           Crypto.Hash.Algorithms (SHA256)
import qualified Crypto.Hash as Cryptonite

import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Tinfoil.Core.Data.Hash

hash :: HashFunction -> ByteString -> Hash
hash SHA256 = hashSHA256

hashSHA256 :: ByteString -> Hash
hashSHA256 bs =
  let dgst = Cryptonite.hash bs :: Cryptonite.Digest SHA256 in
  Hash . BS.pack $ BA.unpack dgst
