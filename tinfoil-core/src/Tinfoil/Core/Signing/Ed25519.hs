{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Tinfoil.Core.Signing.Ed25519 (
    genKeyPair -- re-export from Internal
  , signMessage
  , verifyMessage
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Tinfoil.Core.Data.Key
import           Tinfoil.Core.Data.Signing
import           Tinfoil.Core.Data.Verify
import           Tinfoil.Core.Signing.Ed25519.Internal

-- | Generate a detached Ed25519 signature of a message.
signMessage :: SecretKey Ed25519 -> ByteString -> Maybe' (Signature Ed25519)
signMessage sk msg = do
  sm <- signMessage' sk msg
  pure . Sig_Ed25519 $ BS.take maxSigLen sm

-- | Verify a detached Ed25519 signature of a message.
verifyMessage :: PublicKey Ed25519 -> Signature Ed25519 -> ByteString -> Verified
verifyMessage pk (Sig_Ed25519 sig) msg =
  let sm = sig <> msg in
  verifyMessage' pk sm

