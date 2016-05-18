{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.MAC(
    SigningKey(..)
  , MAC(..)
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

newtype SigningKey =
  SigningKey {
    unSigningKey :: ByteString
  } deriving (Eq, Generic)

instance NFData SigningKey where rnf = genericRnf

-- | Output of a message authentication code algorithm.
-- Do not implement an 'Eq' instance for this type.
newtype MAC =
  MAC {
    unMAC :: ByteString
  } deriving (Show, Generic)

instance NFData MAC where rnf = genericRnf
