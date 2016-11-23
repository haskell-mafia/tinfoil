{-# LANGUAGE NoImplicitPrelude #-}
module Test.IO.Tinfoil.Core.Comparison.Laws where

import           Disorder.Core.IO (testIO)

import           P

import           Test.QuickCheck

import           Tinfoil.Core.Comparison

constEqId :: (ConstEq a) => a -> Property
constEqId x = testIO $ do
  r1 <- x ==# x
  pure $ r1 === True

constEqTrans :: (ConstEq a) => a -> a -> a -> Property
constEqTrans x y z = testIO $ do
  xy <- x ==# y
  yz <- y ==# z
  xz <- x ==# z
  pure $ (xy && yz) ==> (xz === True)

constEqEq :: (Eq a, ConstEq a) => a -> a -> Property
constEqEq x y = testIO $ do
  r1 <- x ==# y
  let r2 = x == y
  pure $ r1 === r2
