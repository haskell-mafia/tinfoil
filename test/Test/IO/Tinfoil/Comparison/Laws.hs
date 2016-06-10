{-# LANGUAGE NoImplicitPrelude #-}
module Test.IO.Tinfoil.Comparison.Laws where

import           Disorder.Core.IO (testIO)

import           P

import           Test.QuickCheck

import           Tinfoil.Comparison

constEqId :: (ConstEq a) => a -> Property
constEqId x = testIO $ do
  r1 <- x ==# x
  pure $ r1 === True
