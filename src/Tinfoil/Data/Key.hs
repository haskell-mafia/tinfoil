{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
module Tinfoil.Data.Key(
    AsymmetricKey(..)
  , Ed25519
  , KeyId(..)
  , SymmetricKey(..)
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

newtype SymmetricKey =
  SymmetricKey {
    unSymmetricKey :: ByteString
  } deriving (Eq, Generic)

instance NFData SymmetricKey where rnf = genericRnf

-- | Identifier for either a symmetric or asymmetric key. Should be
-- globally unique.
newtype KeyId =
  KeyId {
    unKeyId :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData KeyId where rnf = genericRnf

data Ed25519

data AsymmetricKey a where
  Key_Ed25519 :: ByteString -> AsymmetricKey Ed25519
