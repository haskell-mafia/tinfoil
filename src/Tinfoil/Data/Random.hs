{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Random(
    Entropy(..)
) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

-- | Sufficiently-random data. In almost all cases[0], use
-- <https://hackage.haskell.org/package/entropy-0.3.7/docs/System-Entropy.html System.Entropy.getEntropy>.
-- 
-- [0] For completeness, possible exceptions arise when generating key
-- material on Linux extremely early in the boot process (before the
-- network is up).
newtype Entropy =
  Entropy {
    unEntropy :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData Entropy where rnf = genericRnf
