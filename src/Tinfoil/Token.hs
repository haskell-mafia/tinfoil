{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Token (
    secureTokenRaw
  , secureToken
  ) where

import           Data.ByteString (ByteString)

import           P

import           System.IO (IO)

import           Tinfoil.Data.Token
import           Tinfoil.Data.Random (Entropy (..))
import           Tinfoil.Encode (hexEncode)
import           Tinfoil.Random (entropy)

-- | Cryptographically secure identifier, for things like API keys or
-- password reset tokens.  Provided as a raw 'ByteString'.
secureTokenRaw :: IO ByteString
secureTokenRaw =
  unEntropy <$> entropy secureTokenLength

-- | Cryptographically secure identifier, for things like API keys or
-- password reset tokens.  This is a 32 character base-16 string
-- (encoding a 128 bit random token).
secureToken :: IO SecureToken
secureToken =
  SecureToken . hexEncode <$> secureTokenRaw
