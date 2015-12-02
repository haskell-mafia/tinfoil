{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.KDF.Scrypt where

import           Data.ByteString (ByteString)

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Data (Entropy(..))
import           Tinfoil.KDF.Scrypt.Internal


import           Test.Tinfoil.Arbitrary ()
import           Test.QuickCheck

prop_tripping_ScryptParams :: ScryptParams -> Property
prop_tripping_ScryptParams = tripping encodeScryptParams decodeScryptParams

prop_combine_separate :: (ScryptParams, Entropy, ByteString) -> Property
prop_combine_separate = tripping (uncurry3 combine) separate
  where
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f = \(x, y, z) -> f x y z

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
