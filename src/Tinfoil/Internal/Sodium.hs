{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tinfoil.Internal.Sodium (
    Sodium(..)
  , runSodium
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.Internal.Sodium.Data
import           Tinfoil.Internal.Sodium.Foreign

newtype Sodium a =
    Sodium (IO a)
  deriving (Monad, Functor, Applicative)

runSodium :: Sodium a -> IO (Maybe' a)
runSodium (Sodium s) =
  sodiumInit >>= \x -> case x of
    SodiumInitialised ->
      Just' <$> s
    SodiumNotInitialised ->
      pure Nothing'
