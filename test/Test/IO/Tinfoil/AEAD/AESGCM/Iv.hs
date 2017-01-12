{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Tinfoil.AEAD.AESGCM.Iv where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property ((=/=))

import           P

import           System.IO

import           Test.QuickCheck

import           Tinfoil.AEAD.AESGCM.Iv

prop_newInvocationField :: Property
prop_newInvocationField = testIO $ do
  x <- newInvocationField
  y <- newInvocationField
  pure $ x =/= y

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
