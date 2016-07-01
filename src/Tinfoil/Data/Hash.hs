{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Data.Hash(
    Hash(..)
  , HashFunction(..)
  , parseHashFunction
  , renderHash
  , renderHashFunction
) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Encode

-- | Binary representation of a hash.
newtype Hash =
  Hash {
    unHash :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData Hash where rnf = genericRnf

renderHash :: Hash -> Text
renderHash = hexEncode . unHash

-- | Cryptographic hash function designator.
data HashFunction =
    SHA256
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData HashFunction where rnf = genericRnf

renderHashFunction :: HashFunction -> Text
renderHashFunction SHA256 = "SHA256"

parseHashFunction :: Text -> Maybe' HashFunction
parseHashFunction "SHA256" = Just' SHA256
parseHashFunction _ = Nothing'

