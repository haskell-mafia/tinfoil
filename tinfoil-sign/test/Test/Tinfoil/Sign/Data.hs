{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Sign.Data where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Sign.Data

import           Test.Tinfoil.Sign.Arbitrary ()
import           Test.QuickCheck

prop_tripping_SignatureAlgorithm :: SignatureAlgorithm -> Property
prop_tripping_SignatureAlgorithm = tripping renderSignatureAlgorithm parseSignatureAlgorithm

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
