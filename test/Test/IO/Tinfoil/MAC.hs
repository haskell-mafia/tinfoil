{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Tinfoil.MAC where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.IO.Tinfoil
import           Test.Tinfoil.Arbitrary

import           Tinfoil.Data.MAC
import           Tinfoil.Digest
import           Tinfoil.MAC

prop_openssl_hmacSHA256 = forAll genOpenSSLSigningKey $ \key ->
  verifyOpenSSL ["-sha256", "-macopt", "hexkey:" <> (hexKey key), "-mac", "hmac"] (hmacSHA256 key) (T.encodeUtf8 . hexDigest . unMAC)
  where
    hexKey key = T.unpack . hexDigest $ unSigningKey key

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
