{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tinfoil.AEAD.AESGCM.Iv (
    IvError(..)

  , newInvocationField
  , incrementInvocationField
  , newFixedField
  , newGcmIv
  , incrementGcmIv
  ) where

import           Data.Word (Word32)

import           P

import           System.IO (IO)

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.Random

data IvError =
    InvocationCountOverflow
  deriving (Eq, Show)

newInvocationField :: IO InvocationField
newInvocationField =
  InvocationField <$> (RandomField <$> randomWord32) <*> (pure $ InvocationCount 0)

incrementInvocationField :: InvocationField -> Either IvError InvocationField
incrementInvocationField (InvocationField r (InvocationCount n)) =
  let
    n' = n + 1
  in
  case n == ((2 :: Word32)^(32 :: Word32) - 1) of
    True -> Left InvocationCountOverflow
    False -> Right . InvocationField r $ InvocationCount n'

-- | Four random bytes for the fixed field.
newFixedField :: IO FixedField
newFixedField =
  FixedField <$> randomWord32

newGcmIv :: IO GcmIv
newGcmIv =
  GcmIv <$> newFixedField <*> newInvocationField

incrementGcmIv :: GcmIv -> Either IvError GcmIv
incrementGcmIv (GcmIv fixed invoc) =
  incrementInvocationField invoc >>= \invoc' ->
    pure $ GcmIv fixed invoc'
