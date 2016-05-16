{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Tinfoil.Comparison where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Disorder.Core.IO (testIO)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Tinfoil.Comparison

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_safeEq_positive :: ByteString -> Property
prop_safeEq_positive bs1 = testIO $
  let bs2 = BS.copy bs1 in do
  r1 <- safeEq bs1 bs1
  r2 <- safeEq bs1 bs2
  pure $ (r1, r2) === (True, True)

prop_safeEq_negative :: UniquePair ByteString -> Property
prop_safeEq_negative (UniquePair a b) = testIO $ do
  r <- safeEq a b
  pure $ r === False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
