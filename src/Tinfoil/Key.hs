{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Key(
    genSymmetricKey256
) where

import           P

import           System.IO (IO)

import           Tinfoil.Data.Key
import           Tinfoil.Data.Random
import           Tinfoil.Random

-- | Generate a 256-bit symmetric cryptographic key.
genSymmetricKey256 :: IO SymmetricKey
genSymmetricKey256 = fmap (SymmetricKey . unEntropy) $ entropy 32

