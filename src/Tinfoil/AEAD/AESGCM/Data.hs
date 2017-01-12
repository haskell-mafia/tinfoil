{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Tinfoil.AEAD.AESGCM.Data (
    GcmIv(..)

  , FixedField(..)

  , InvocationField(..)
  , RandomField(..)
  , InvocationCount(..)
  ) where

import            Data.Word (Word32)

import            P

-- | This is a truncated hash identifying the context of encryption - machine
-- and target file.
newtype FixedField =
  FixedField {
    unFixedField :: Word32
  } deriving (Eq, Show, Ord, Num, Bounded)

-- | Four random bytes from a CSPRNG.
newtype RandomField =
  RandomField {
    unRandomField :: Word32
  } deriving (Eq, Show, Num, Enum, Bounded)

-- | Number of invocations of the encryption function with the same key since
-- startup.
newtype InvocationCount =
  InvocationCount {
    unInvocationCount :: Integer
  } deriving (Eq, Show, Ord, Num)

-- | This identifies the operation relative to the context. It consists of
-- a random value (32b) and an incrementing counter (32b).
data InvocationField =
    InvocationField !RandomField !InvocationCount
  deriving (Eq, Show)

-- | IV/nonce for GCM. 96 bits in total. Must never repeat with the same key.
data GcmIv =
    GcmIv !FixedField !InvocationField
  deriving (Eq, Show)
