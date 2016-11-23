{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Core.Data.KDF where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Core.Data.KDF

import           Test.QuickCheck
import           Test.Tinfoil.Core.Arbitrary ()
import           Test.Tinfoil.Core.Gen

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
