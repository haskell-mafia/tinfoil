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
