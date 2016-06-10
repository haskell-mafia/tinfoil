{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.IO.Tinfoil.Data.MAC where

import qualified Data.ByteString.Char8 as BSC

import           P

import           System.IO

import           Tinfoil.Data.MAC

import           Test.IO.Tinfoil.Comparison.Laws
import           Test.Tinfoil.Arbitrary ()
import           Test.QuickCheck

prop_MAC_ConstEqId :: MAC -> Property
prop_MAC_ConstEqId = constEqId

prop_MAC_ConstEqEq :: MAC -> MAC -> Property
prop_MAC_ConstEqEq = constEqEq

-- | Can't use real random bytestrings here as the probability of three
-- identical values is tiny.
prop_MAC_ConstEqTrans :: Bool -> Bool -> Bool -> Property
prop_MAC_ConstEqTrans x y z =
  let x' = MAC . BSC.pack $ show x
      y' = MAC . BSC.pack $ show y
      z' = MAC . BSC.pack $ show z in
  constEqTrans x' y' z'

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
