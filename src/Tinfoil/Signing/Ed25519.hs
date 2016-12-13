{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Tinfoil.Signing.Ed25519 (
    genKeyPair -- re-export from Internal
  , signMessage
  , verifyMessage
) where

import           Data.ByteString (ByteString)

import           P

import           Tinfoil.Data.Key
import           Tinfoil.Data.Signing
import           Tinfoil.Data.Verify
import           Tinfoil.Signing.Ed25519.Internal

-- | Generate a detached Ed25519 signature of a message.
signMessage :: SecretKey Ed25519 -> ByteString -> Maybe' (Signature Ed25519)
signMessage sk msg =
  Sig_Ed25519 <$> signMessage' sk msg

-- | Verify a detached Ed25519 signature of a message.
verifyMessage :: PublicKey Ed25519 -> Signature Ed25519 -> ByteString -> Verified
verifyMessage pk (Sig_Ed25519 sig) msg =
  verifyMessage' pk sig msg

