{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Tinfoil.Internal.Sodium.Foreign where

import           Disorder.Core.IO (testIO)

import           P

import           System.IO

import           Test.QuickCheck

import           Tinfoil.Internal.Sodium.Data
import           Tinfoil.Internal.Sodium.Foreign

prop_sodiumInit :: Property
prop_sodiumInit = once . testIO $ do
  r1 <- sodiumInit
  r2 <- sodiumInit
  pure $ (r1, r2) === (SodiumInitialised, SodiumInitialised)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
