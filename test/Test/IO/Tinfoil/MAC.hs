{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Tinfoil.MAC where

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Core.IO (testIO)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.IO.Tinfoil
import           Test.Tinfoil.Gen
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.Data
import           Tinfoil.Encode
import           Tinfoil.MAC

prop_openssl_hmacSHA256 = forAll genOpenSSLSymmetricKey $ \key ->
  verifyOpenSSL ["-sha256", "-macopt", "hexkey:" <> (hexKey key), "-mac", "hmac"] (hmacSHA256 key) (T.encodeUtf8 . hexEncode . unMAC)
  where
    hexKey key = T.unpack . hexEncode $ unSymmetricKey key

prop_verifyMAC :: KeyedHashFunction -> UniquePair SymmetricKey -> UniquePair ByteString -> Property
prop_verifyMAC khf (UniquePair sk1 sk2) (UniquePair bs1 bs2) =
  let sig = macBytes khf sk1 bs1 in testIO $ do
  r1 <- verifyMAC khf sk1 bs1 sig -- good
  r2 <- verifyMAC khf sk2 bs1 sig -- bad key
  r3 <- verifyMAC khf sk1 bs2 sig -- bad message
  r4 <- verifyMAC khf sk2 bs2 sig -- bad key, bad message
  pure $ (r1, r2, r3, r4) === (Verified, NotVerified, NotVerified, NotVerified)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
