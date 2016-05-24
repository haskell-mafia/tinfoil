{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.MAC(
    SigningKey(..)
  , MAC(..)
  , KeyedHashFunction(..)
  , parseKeyedHashFunction
  , renderKeyedHashFunction
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

-- | Keyed-hash algorithm designator, for inclusion as a request parameter.
data KeyedHashFunction =
    HMAC_SHA256
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData KeyedHashFunction where rnf = genericRnf

renderKeyedHashFunction :: KeyedHashFunction -> Text
renderKeyedHashFunction HMAC_SHA256 = "HMAC-SHA256"

parseKeyedHashFunction :: Text -> Maybe KeyedHashFunction
parseKeyedHashFunction "HMAC-SHA256" = pure HMAC_SHA256
parseKeyedHashFunction _ = Nothing
