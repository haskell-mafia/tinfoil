{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Signing(
    SignatureAlgorithm(..)
  , SignatureVersion(..)
  , parseSignatureAlgorithm
  , parseSignatureVersion
  , renderSignatureAlgorithm
  , renderSignatureVersion
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

data SignatureVersion =
    SignatureV1
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData SignatureVersion where rnf = genericRnf

renderSignatureVersion :: SignatureVersion -> Text
renderSignatureVersion SignatureV1 = "v1"

parseSignatureVersion :: Text -> Maybe SignatureVersion
parseSignatureVersion "v1" = pure SignatureV1
parseSignatureVersion _ = Nothing

-- | Supported digital signature algorithms.
data SignatureAlgorithm =
    Sign_Ed25519
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData SignatureAlgorithm where rnf = genericRnf

renderSignatureAlgorithm :: SignatureAlgorithm -> Text
renderSignatureAlgorithm Sign_Ed25519 = "Sign-Ed25519"

parseSignatureAlgorithm :: Text -> Maybe' SignatureAlgorithm
parseSignatureAlgorithm "Sign-Ed25519" = pure Sign_Ed25519
parseSignatureAlgorithm _ = Nothing'

