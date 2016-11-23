{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Core.KDF.Scrypt where

import           Data.ByteString (ByteString)

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Core.Data (Entropy(..))
import           Tinfoil.Core.KDF.Scrypt
import           Tinfoil.Core.KDF.Scrypt.Internal

import           Test.Tinfoil.Core.Arbitrary ()
import           Test.Tinfoil.Core.Gen
import           Test.QuickCheck

prop_tripping_ScryptParams :: ScryptParams -> Property
prop_tripping_ScryptParams = tripping encodeScryptParams decodeScryptParams

prop_combine_separate :: (ScryptParams, Entropy, ByteString) -> Property
prop_combine_separate = tripping (uncurry3 combine) separate
  where
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f = \(x, y, z) -> f x y z

prop_paramsUpToDate_bad :: Property
prop_paramsUpToDate_bad = forAll genInvalidCredentialHash $ \h ->
  let r = paramsUpToDate defaultParams h in
  r === Nothing'

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
