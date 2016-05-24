{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Hash where

import           Data.ByteString (ByteString)

import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Tinfoil.Hash

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_hashSHA256 :: UniquePair ByteString -> Property
prop_hashSHA256 (UniquePair bs1 bs2) =
  let h1 = hashSHA256 bs1
      h2 = hashSHA256 bs2 in
  (h1 == h2) === False

prop_hashSHA256_reftrans :: ByteString -> Property
prop_hashSHA256_reftrans bs =
  let h1 = hashSHA256 bs
      h2 = hashSHA256 bs in
  h1 === h2

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
