{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
module Tinfoil.Data.Key(
    Ed25519
  , KeyId(..)
  , PublicKey(..)
  , SecretKey(..)
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

data PublicKey a where
  PKey_Ed25519 :: ByteString -> PublicKey Ed25519

instance Eq (PublicKey a) where
  (PKey_Ed25519 x) == (PKey_Ed25519 y) = x == y

instance NFData (PublicKey a) where
  rnf (PKey_Ed25519 x) = rnf x

data SecretKey a where
  SKey_Ed25519 :: ByteString -> SecretKey Ed25519

instance Eq (SecretKey a) where
  (SKey_Ed25519 x) == (SKey_Ed25519 y) = x == y

instance NFData (SecretKey a) where
  rnf (SKey_Ed25519 x) = rnf x
