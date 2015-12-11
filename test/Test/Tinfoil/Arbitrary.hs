{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tinfoil.Arbitrary where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           P

import           Test.QuickCheck

import           Tinfoil.Random

instance Arbitrary a => Arbitrary (NonEmpty a)
  where
    arbitrary =
      NE.fromList <$> listOf1 arbitrary

credentialLength :: Gen Int
credentialLength = choose (0, 512)

excludedChars :: Gen [Char]
excludedChars = arbitrary `suchThat` (/= (NE.toList credentialCharSet))

newtype DrawBits =
  DrawBits {
    unDrawBits :: [Bool]
  } deriving (Eq, Show)

instance Arbitrary DrawBits where
  arbitrary = do
    n <- choose (0, 63)
    DrawBits <$> vectorOf n arbitrary

-- For tests involving floating-point computations (e.g., logs) which don't
-- retain sufficient precision close to 64 bits.
drawOnes32 :: Gen [Bool]
drawOnes32 = do
  n <- choose (1, 32)
  vectorOf n $ pure True

drawZeroes32 :: Gen [Bool]
drawZeroes32 = do
  n <- choose (1, 32)
  vectorOf n $ pure False
