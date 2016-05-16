{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Tinfoil.Hash where

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.IO.Tinfoil

import           Tinfoil.Hash

prop_openssl_hashSHA256 = verifyOpenSSL ["-sha256"] hashSHA256 (T.encodeUtf8 . hexDigest . unHash)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
