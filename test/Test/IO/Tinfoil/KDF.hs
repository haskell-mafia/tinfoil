{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Tinfoil.KDF where

import           Data.ByteString (ByteString)

import           Disorder.Core.IO (testIO)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Tinfoil.KDF

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_safeEq_positive :: ByteString -> Property
prop_safeEq_positive bs = testIO $ do
  r <- safeEq bs bs
  pure $ r === True

prop_safeEq_negative :: UniquePair ByteString -> Property
prop_safeEq_negative (UniquePair a b) = testIO $ do
  r <- safeEq a b
  pure $ r === False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 } )
