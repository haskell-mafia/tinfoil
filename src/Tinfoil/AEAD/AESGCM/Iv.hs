{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tinfoil.AEAD.AESGCM.Iv (
    IvError(..)

  , newInvocationField
  , incrementInvocationField
  , newFixedField
  ) where

import           Data.Word (Word32)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BSL

import           P

import           System.IO (IO)

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.Data.Hash
import           Tinfoil.Data.Random
import           Tinfoil.Hash
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

-- | Mix the encryption context with a machine identifier (random ID), hash it
-- and truncate to get our fixed field.
newFixedField :: EncryptionContext -> IO FixedField
newFixedField (EncryptionContext ec) = do
  (Entropy mid) <- entropy 4
  let
    dat = unHash . hashSHA256 $ ec <> mid
    fixed = B.runGet B.getWord32le . BSL.fromChunks $ pure dat
  pure $ FixedField fixed
