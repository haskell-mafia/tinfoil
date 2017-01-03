{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Tinfoil.Internal.Sodium.Foreign (
    sodiumInit
  ) where

import           Foreign.C (CInt(..))

import           P

import           System.IO (IO)

import           Tinfoil.Internal.Sodium.Data

-- |
-- Performs global init stuff in the C library. Some libsodium functionality
-- can be used without this function being called, but will generally result
-- in loss of thread-safety - in other words, don't do it.
--
-- This function is thread-safe and idempotent.
foreign import ccall safe "sodium_init" sodium_init
  :: IO CInt

sodiumInit :: IO SodiumInitStatus
sodiumInit =
  sodium_init >>= \x -> case x of
    0 -> pure SodiumInitialised -- init succeeded
    1 -> pure SodiumInitialised -- already initialised
    _ -> pure SodiumNotInitialised

