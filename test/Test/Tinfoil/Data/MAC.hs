{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Data.MAC where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Data.MAC

import           Test.Tinfoil.Arbitrary ()
import           Test.QuickCheck

prop_tripping_KeyedHashFunction :: KeyedHashFunction -> Property
prop_tripping_KeyedHashFunction = tripping renderKeyedHashFunction parseKeyedHashFunction

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
