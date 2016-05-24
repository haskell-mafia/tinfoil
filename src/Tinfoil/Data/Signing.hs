{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Signing(
    KeyId(..)
  , RequestScope(..)
  , SignatureVersion(..)
  , parseSignatureVersion
  , renderSignatureVersion
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

data SignatureVersion =
    SignatureV1
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData SignatureVersion where rnf = genericRnf

renderSignatureVersion :: SignatureVersion -> Text
renderSignatureVersion SignatureV1 = "v1"

parseSignatureVersion :: Text -> Maybe SignatureVersion
parseSignatureVersion "v1" = pure SignatureV1
parseSignatureVersion _ = Nothing

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
