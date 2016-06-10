{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.MAC(
    KeyedHashFunction(..)
  , MAC(..)
  , keyHashFunction
  , parseKeyedHashFunction
  , renderKeyedHashFunction
  , renderMAC
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Data.Hash
import           Tinfoil.Encode

-- | Output of a message authentication code algorithm.
-- Do not implement an 'Eq' instance for this type.
newtype MAC =
  MAC {
    unMAC :: ByteString
  } deriving (Show, Generic)

instance NFData MAC where rnf = genericRnf

-- | Hexadecimal encoding of a MAC.
renderMAC :: MAC -> Text
renderMAC = hexEncode . unMAC

-- | Keyed-hash algorithm designator, for inclusion as a request
-- parameter.
data KeyedHashFunction =
    HMAC_SHA256
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData KeyedHashFunction where rnf = genericRnf

-- | Provide the corresponding KHF for a given cryptographic hash function.
keyHashFunction :: HashFunction -> KeyedHashFunction
keyHashFunction SHA256 = HMAC_SHA256

renderKeyedHashFunction :: KeyedHashFunction -> Text
renderKeyedHashFunction HMAC_SHA256 = "HMAC-SHA256"

parseKeyedHashFunction :: Text -> Maybe' KeyedHashFunction
parseKeyedHashFunction "HMAC-SHA256" = pure HMAC_SHA256
parseKeyedHashFunction _ = Nothing'
