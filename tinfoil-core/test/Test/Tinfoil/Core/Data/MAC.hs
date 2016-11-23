{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Core.Data.MAC where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Core.Data.MAC

import           Test.Tinfoil.Core.Arbitrary ()
import           Test.QuickCheck

prop_tripping_KeyedHashFunction :: KeyedHashFunction -> Property
prop_tripping_KeyedHashFunction = tripping renderKeyedHashFunction parseKeyedHashFunction

prop_tripping_MAC :: MAC -> Property
prop_tripping_MAC = tripping renderMAC parseMAC

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
