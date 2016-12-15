{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tinfoil.Internal.Sodium (
    Sodium(..)
  , runSodium
  ) where

import           Control.Monad.IO.Class (liftIO)

import           P

import           System.IO (IO)

import           Tinfoil.Internal.Sodium.Data
import           Tinfoil.Internal.Sodium.Foreign

import           X.Control.Monad.Trans.Either (EitherT, left)

data SodiumError =
    SodiumInitFailed
  | NoAESGCMCPUSupport
  deriving (Eq, Show)

newtype Sodium a =
    Sodium (IO a)
  deriving (Monad, Functor, Applicative)

runSodium :: Sodium a -> EitherT SodiumError IO a
runSodium (Sodium s) =
  (liftIO sodiumInit) >>= \x -> case x of
    SodiumInitialised ->
      (liftIO aesgcmSupported) >>= \y -> case y of
        AESGCMSupported ->
          liftIO s
        AESGCMNotSupported ->
          left NoAESGCMCPUSupport
    SodiumNotInitialised ->
      left SodiumInitFailed
