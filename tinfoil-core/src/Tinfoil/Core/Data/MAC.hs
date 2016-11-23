{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.MAC(
    KeyedHashFunction(..)
  , MAC(..)
  , keyHashFunction
  , parseKeyedHashFunction
  , parseMAC
  , renderKeyedHashFunction
  , renderMAC
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.Binary (Binary)
import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Core.Comparison
import           Tinfoil.Core.Data.Hash
import           Tinfoil.Core.Encode

-- | Output of a message authentication code algorithm.
-- Do not implement an 'Eq' instance for this type.
newtype MAC =
  MAC {
    unMAC :: ByteString
  } deriving (Show, Generic, Binary, ConstEq)

instance NFData MAC where rnf = genericRnf

-- | Hexadecimal encoding of a MAC.
renderMAC :: MAC -> Text
renderMAC = hexEncode . unMAC

-- | Parse the hexadecimal encoding of a MAC.
parseMAC :: Text -> Maybe' MAC
parseMAC t = MAC <$> hexDecode 32 t

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
