{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Tinfoil.Core.Signing.Ed25519.Internal where

import           P

import           System.IO

import           Tinfoil.Core.Signing.Ed25519.Internal

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

-- Check these don't change on us.
prop_ed25519_lengths =
  once $ (pubKeyLen, secKeyLen, maxSigLen) === (32, 64, 64)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
