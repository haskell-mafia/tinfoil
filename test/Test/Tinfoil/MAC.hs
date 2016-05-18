{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Tinfoil.MAC where

import           Data.ByteString (ByteString)

import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Tinfoil.Data.MAC
import           Tinfoil.MAC

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary ()

prop_hmacSHA256_key_mismatch :: UniquePair SigningKey -> ByteString -> Property
prop_hmacSHA256_key_mismatch (UniquePair k1 k2) msg =
  let h1 = hmacSHA256 k1 msg
      h2 = hmacSHA256 k2 msg in
  (h1 == h2) === False

prop_hmacSHA256_msg_mismatch :: SigningKey -> UniquePair ByteString -> Property
prop_hmacSHA256_msg_mismatch k (UniquePair msg1 msg2) =
  let h1 = hmacSHA256 k msg1
      h2 = hmacSHA256 k msg2 in
  (h1 == h2) === False

prop_hmacSHA256_reftrans :: SigningKey -> ByteString -> Property
prop_hmacSHA256_reftrans k msg =
  let h1 = hmacSHA256 k msg
      h2 = hmacSHA256 k msg in
  h1 === h2

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
