{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tinfoil.Sign.Arbitrary where

import           P

import           Test.QuickCheck

import           Tinfoil.Sign.Data

instance Arbitrary SignatureAlgorithm where
  arbitrary = elements [minBound..maxBound]
