{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Data.Key where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Data.Key

import           Test.QuickCheck
import           Test.Tinfoil.Arbitrary ()

prop_tripping_SymmetricKey :: SymmetricKey -> Property
prop_tripping_SymmetricKey = tripping renderSymmetricKey parseSymmetricKey

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
