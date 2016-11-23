{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.Signing(
    Signature(..)
  , SignatureAlgorithm(..)
  , parseSignatureAlgorithm
  , renderSignatureAlgorithm
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Core.Data.Key

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

data Signature a where
    Sig_Ed25519 :: ByteString -> Signature Ed25519

instance Eq (Signature a) where
  (Sig_Ed25519 x) == (Sig_Ed25519 y) = x == y

instance NFData (Signature a) where
  rnf (Sig_Ed25519 x) = rnf x

