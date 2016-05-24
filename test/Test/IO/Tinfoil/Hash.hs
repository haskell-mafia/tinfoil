{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Tinfoil.Hash where

import qualified Data.Text.Encoding as T

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.IO.Tinfoil

import           Tinfoil.Data.Hash
import           Tinfoil.Encode
import           Tinfoil.Hash

prop_openssl_hashSHA256 = verifyOpenSSL ["-sha256"] hashSHA256 (T.encodeUtf8 . hexEncode . unHash)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
