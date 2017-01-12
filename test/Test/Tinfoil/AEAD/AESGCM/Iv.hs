{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Tinfoil.AEAD.AESGCM.Iv where

import           Disorder.Core.Property ((=/=))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.AEAD.AESGCM.Iv

prop_incrementInvocationField :: InvocationField -> Property
prop_incrementInvocationField f =
  (incrementInvocationField f) =/= (Right f)
  

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
