{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Tinfoil.MAC where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as T

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.IO.Tinfoil

import           Tinfoil.Data.MAC
import           Tinfoil.Digest
import           Tinfoil.MAC

prop_openssl_hmacSHA256 key =
  verifyOpenSSL ["-sha256", "-hmac", BSC.unpack key] (hmacSHA256 (SigningKey key)) (T.encodeUtf8 . hexDigest . unMAC)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
