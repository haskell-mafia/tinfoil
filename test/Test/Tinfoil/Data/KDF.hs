{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Data.KDF where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Data.KDF

import           Test.Tinfoil.Arbitrary
import           Test.QuickCheck

prop_tripping_MCFPrefix :: MCFPrefix -> Property
prop_tripping_MCFPrefix = tripping renderMCFPrefix parseMCFPrefix

prop_tripping_MCFHash :: Property
prop_tripping_MCFHash = forAll ((,) <$> arbitrary <*> genInvalidCredentialHash) $ \mcfPair ->
  let mcfh = uncurry packMCFHash mcfPair
      mcfPair' = unpackMCFHash mcfh in
  mcfPair' === Just' mcfPair

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
