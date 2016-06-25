{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Key(
    genSymmetricKey
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.Data.Key
import           Tinfoil.Data.Random
import           Tinfoil.Random

-- | Generate a 256-bit symmetric cryptographic key.
genSymmetricKey :: IO SymmetricKey
genSymmetricKey = fmap (SymmetricKey . unEntropy) $ entropy symmetricKeyLength
