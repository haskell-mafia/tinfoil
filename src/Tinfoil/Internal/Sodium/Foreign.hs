{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Tinfoil.Internal.Sodium.Foreign (
    sodiumInit
  , aesgcmSupported
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
-- This function is thread-safe and idempotent. It doesn't allocate anything on
-- the heap, but does keep one file descriptor open (for /dev/urandom); it
-- will only do this once.
foreign import ccall safe "sodium_init" sodium_init
  :: IO CInt

sodiumInit :: IO SodiumInitStatus
sodiumInit =
  sodium_init >>= \x -> case x of
    0 -> pure SodiumInitialised -- init succeeded
    1 -> pure SodiumInitialised -- already initialised
    _ -> pure SodiumNotInitialised

-- |
-- Whether or not the CPU supports the x86 extensions required for
-- hardware-accelerated AES-GCM (the only kind libsodium supports).
--
-- sodium_init must be called before this function.
aesgcmSupported :: IO AESGCMSupport
aesgcmSupported =
  sodium_aesgcm_is_available >>= \x -> case x of
    1 -> pure AESGCMSupported
    _ -> pure AESGCMNotSupported

foreign import ccall safe "crypto_aead_aes256gcm_is_available" sodium_aesgcm_is_available
  :: IO CInt
