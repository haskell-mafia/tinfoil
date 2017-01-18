{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Tinfoil.AEAD.AESGCM.Iv where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property ((=/=))
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.AEAD.AESGCM.Iv

prop_newInvocationField :: Property
prop_newInvocationField = testIO $ do
  x <- newInvocationField
  y <- newInvocationField
  pure $ x =/= y

prop_newFixedField :: UniquePair EncryptionContext -> Property
prop_newFixedField (UniquePair ec1 ec2) = testIO $ do
  x <- newFixedField ec1
  y <- newFixedField ec2
  pure $ x =/= y

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
