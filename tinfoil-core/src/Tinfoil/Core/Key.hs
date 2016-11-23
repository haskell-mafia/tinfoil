{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Core.Key(
    genSymmetricKey
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.Core.Data.Key
import           Tinfoil.Core.Data.Random
import           Tinfoil.Core.Random

-- | Generate a 256-bit symmetric cryptographic key.
genSymmetricKey :: IO SymmetricKey
genSymmetricKey = fmap (SymmetricKey . unEntropy) $ entropy symmetricKeyLength
