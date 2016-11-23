{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Tinfoil.Core.KDF.Scrypt.Compat where

import           Disorder.Core.IO (testIO)

import           P

import           System.IO

import           Tinfoil.Core.Data
import           Tinfoil.Core.KDF.Scrypt


import           Test.Tinfoil.Core.Arbitrary ()
import           Test.QuickCheck

prop_verifyCredential_consistency :: Property
prop_verifyCredential_consistency =
  let hsh = CredentialHash "16|8|12|GtrEakXBx+JO41wYsSlgv07Mmf+rBW78+uapzAbQD+A=|SPTABdNBoaD8hoGMzJ68kLZFbKHdUwIatC0ufgif6SugdYhD8pm4TrIsxs+6FzO7J2cwmpeL/4oIER4obZr0ng=="
      pass = Credential "foo" in testIO $ do
  r <- verifyCredential hsh pass
  pure $ r === Verified

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 } )
