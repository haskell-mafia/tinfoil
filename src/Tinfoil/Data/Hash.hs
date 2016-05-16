{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Hash(
    Hash(..)
  , hexDigest
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as T

import           GHC.Generics (Generic)

import           P

-- | Binary representation of a hash.
newtype Hash =
  Hash {
    unHash :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData Hash

hexDigest :: Hash -> Text
hexDigest = T.decodeUtf8 . Base16.encode . unHash
