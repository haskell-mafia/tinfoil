{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Tinfoil.AEAD.AESGCM.Data (
    GcmIv(..)
  , packGcmIv

  , FixedField(..)

  , InvocationField(..)
  , packInvocationField
  , RandomField(..)
  , InvocationCount(..)
  ) where

import            Data.Bits (shiftL, (.|.))
import            Data.Word (Word32, Word64)
import            Data.Word (Word32)
import qualified Data.Binary.Put as B
import           Data.Bits (shiftL, (.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32, Word64)

import           P

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
    unInvocationCount :: Word32
  } deriving (Eq, Show, Ord, Num)

-- | This identifies the operation relative to the context. It consists of
-- a random value (32b) and an incrementing counter (32b).
data InvocationField =
    InvocationField !RandomField !InvocationCount
  deriving (Eq, Show)

packInvocationField :: InvocationField -> Word64
packInvocationField (InvocationField (RandomField rf) (InvocationCount ic)) =
  -- ic is guaranteed to be non-negative and less than 2^32.
  ((fromIntegral rf :: Word64) `shiftL` 32) .|. (fromIntegral ic)

-- | IV/nonce for GCM. 96 bits in total. Must never repeat with the same key.
data GcmIv =
    GcmIv !FixedField !InvocationField
  deriving (Eq, Show)

packGcmIv :: GcmIv -> ByteString
packGcmIv (GcmIv fixed invoc) =
  BSL.toStrict . B.runPut $ do
    B.putWord32le $ unFixedField fixed
    B.putWord64le $ packInvocationField invoc
