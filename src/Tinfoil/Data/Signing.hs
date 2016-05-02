{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Signing(
    KeyedHashFunction(..)
  , RequestScope(..)
  , SignatureVersion(..)
  , SigningKey(..)
  , parseKeyedHashFunction
  , parseSignatureVersion
  , renderKeyedHashFunction
  , renderSignatureVersion
  ) where

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

data SignatureVersion =
    SignatureV1
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData SignatureVersion

renderSignatureVersion :: SignatureVersion -> Text
renderSignatureVersion SignatureV1 = "v1"

parseSignatureVersion :: Text -> Maybe SignatureVersion
parseSignatureVersion "v1" = pure SignatureV1
parseSignatureVersion _ = Nothing

-- | Keyed-hash algorithm designator, for inclusion as a request parameter.
data KeyedHashFunction =
    HMAC_SHA256
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData KeyedHashFunction

renderKeyedHashFunction :: KeyedHashFunction -> Text
renderKeyedHashFunction HMAC_SHA256 = "HMAC-SHA256"

parseKeyedHashFunction :: Text -> Maybe KeyedHashFunction
parseKeyedHashFunction "HMAC-SHA256" = pure HMAC_SHA256
parseKeyedHashFunction _ = Nothing

newtype SigningKey =
  SigningKey {
    unSigningKey :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData SigningKey

newtype RequestScope =
  RequestScope {
    unRequestScope :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData RequestScope
