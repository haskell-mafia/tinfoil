{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Tinfoil.Signing where

import           Data.ByteString (ByteString)

import           Disorder.Core (UniquePair(..))
import           Disorder.Core.IO (testIO)

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.Data
import           Tinfoil.Signing

prop_verifyBytes :: KeyedHashFunction -> UniquePair SymmetricKey -> UniquePair ByteString -> Property
prop_verifyBytes khf (UniquePair sk1 sk2) (UniquePair bs1 bs2) =
  let sig = signBytes khf sk1 bs1 in testIO $ do
  r1 <- verifyBytes khf sk1 bs1 sig -- good
  r2 <- verifyBytes khf sk2 bs1 sig -- bad key
  r3 <- verifyBytes khf sk1 bs2 sig -- bad message
  r4 <- verifyBytes khf sk2 bs2 sig -- bad key, bad message
  pure $ (r1, r2, r3, r4) === (Verified, NotVerified, NotVerified, NotVerified)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
