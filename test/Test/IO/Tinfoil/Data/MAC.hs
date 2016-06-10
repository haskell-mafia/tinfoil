{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.IO.Tinfoil.Data.MAC where

import           P

import           System.IO

import           Tinfoil.Data.MAC

import           Test.IO.Tinfoil.Comparison.Laws
import           Test.Tinfoil.Arbitrary ()
import           Test.QuickCheck

prop_ConstEqId :: MAC -> Property
prop_ConstEqId = constEqId

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
