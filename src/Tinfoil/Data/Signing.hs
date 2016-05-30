{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Signing(
    SignatureAlgorithm(..)
  , parseSignatureAlgorithm
  , renderSignatureAlgorithm
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

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
