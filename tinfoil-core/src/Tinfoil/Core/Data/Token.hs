{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.Token(
    SecureToken (..)
  , secureTokenLength
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

-- | Cryptographically secure identifier, for things like API keys or
-- password reset tokens.  This is a 32 character base-16 string
-- (encoding a 128 bit random token).
newtype SecureToken =
  SecureToken {
      unSecureToken :: Text
    } deriving (Show, Generic)

instance NFData SecureToken where rnf = genericRnf

secureTokenLength :: Int
secureTokenLength =
  16
