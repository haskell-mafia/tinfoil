{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.MAC(
    SigningKey(..)
  , MAC(..)
  ) where

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

newtype SigningKey =
  SigningKey {
    unSigningKey :: ByteString
  } deriving (Eq, Generic)

instance NFData SigningKey

-- | Output of a message authentication code algorithm.
--
-- Do not implement an 'Eq' instance for this type.
newtype MAC =
  MAC {
    unMAC :: ByteString
  } deriving (Generic)

instance NFData MAC
