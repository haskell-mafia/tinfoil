{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Core.Token (
    secureTokenRaw
  , secureToken
  ) where

import           Data.ByteString (ByteString)

import           P

import           System.IO (IO)

import           Tinfoil.Core.Data.Token
import           Tinfoil.Core.Data.Random (Entropy (..))
import           Tinfoil.Core.Encode (hexEncode)
import           Tinfoil.Core.Random (entropy)

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
