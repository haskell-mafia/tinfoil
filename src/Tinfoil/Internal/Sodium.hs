{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tinfoil.Internal.Sodium (
    SodiumError(..)
  , SodiumInitMarker
  , initialiseSodium
  ) where

import           Control.Monad.IO.Class (liftIO)

import           P

import           System.IO (IO)

import           Tinfoil.Internal.Sodium.Data
import qualified Tinfoil.Internal.Sodium.Foreign as F

import           X.Control.Monad.Trans.Either (EitherT, left)

data SodiumError =
    SodiumInitFailed
  | NoAESGCMCPUSupport
  deriving (Eq, Show)

-- | Marker type indicating that the sodium C library has been
-- successfully initialised. Every function which calls out to
-- libsodium should require this as an argument.
data SodiumInitMarker =
    -- | `sodium_init()` has been called successfully, and we're running on a
    -- CPU which meets our requirements (aesni).
    SodiumIsInitialised
  deriving (Eq, Show)

-- | Perform required initialisation required for the sodium C library. This
-- includes ensuring we have hardware support for aes-gcm (can't run safely
-- without it).
--
-- This function must be called before calling out to anything else in
-- libsodium.
initialiseSodium :: EitherT SodiumError IO SodiumInitMarker
initialiseSodium =
  (liftIO F.sodiumInit) >>= \x -> case x of
    SodiumInitialised ->
      (liftIO F.aesgcmSupported) >>= \y -> case y of
        AESGCMSupported ->
          pure SodiumIsInitialised
        AESGCMNotSupported ->
          left NoAESGCMCPUSupport
    SodiumNotInitialised ->
      left SodiumInitFailed
