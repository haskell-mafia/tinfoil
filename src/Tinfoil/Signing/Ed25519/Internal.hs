{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Tinfoil.Signing.Ed25519.Internal (
    pubKeyLen
  , secKeyLen
) where

import           Foreign.C

import           P

pubKeyLen :: Int
pubKeyLen = fromIntegral tinfoil_sodium_pubkey_len

foreign import ccall unsafe tinfoil_sodium_pubkey_len
  :: CInt

secKeyLen :: Int
secKeyLen = fromIntegral tinfoil_sodium_seckey_len

foreign import ccall unsafe tinfoil_sodium_seckey_len
  :: CInt
