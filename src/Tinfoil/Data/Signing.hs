{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Signing(
    KeyedHashFunction(..)
  , KeyId(..)
  , RequestScope(..)
  , Signature(..)
  , SignatureVersion(..)
  , parseKeyedHashFunction
  , parseSignatureVersion
  , renderKeyedHashFunction
  , renderSignatureVersion
  , signatureBytes
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Data.MAC

data SignatureVersion =
    SignatureV1
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData SignatureVersion where rnf = genericRnf

renderSignatureVersion :: SignatureVersion -> Text
renderSignatureVersion SignatureV1 = "v1"

parseSignatureVersion :: Text -> Maybe SignatureVersion
parseSignatureVersion "v1" = pure SignatureV1
parseSignatureVersion _ = Nothing

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

newtype KeyId =
  KeyId {
    unKeyId :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData KeyId where rnf = genericRnf

newtype RequestScope =
  RequestScope {
    unRequestScope :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData RequestScope where rnf = genericRnf

newtype Signature =
  Signature {
    unSignature :: MAC
  } deriving (Show, Generic)

instance NFData Signature where rnf = genericRnf

signatureBytes :: Signature -> ByteString
signatureBytes = unMAC . unSignature
