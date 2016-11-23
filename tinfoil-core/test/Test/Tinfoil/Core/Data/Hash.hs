{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Tinfoil.Core.Data.Hash where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Core.Data.Hash

import           Test.Tinfoil.Core.Arbitrary ()
import           Test.QuickCheck

prop_tripping_HashFunction :: HashFunction -> Property
prop_tripping_HashFunction = tripping renderHashFunction parseHashFunction

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
