{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Tinfoil.Core where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Disorder.Core.IO (testIO)

import           P

import qualified Prelude

import           System.IO
import           System.IO.Temp (withTempFile)
import           System.Process (readProcess)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Core.Arbitrary ()

verifyOpenSSL :: [Prelude.String]
              -> (ByteString -> a)
              -> (a -> ByteString)
              -> ByteString
              -> Property
verifyOpenSSL args hf final inp = testIO $ withTempFile "." "tinfoil-" $ \fp h -> do
  BS.hPut h inp
  hFlush h
  out <- fmap BSC.pack $ readProcess "/usr/bin/openssl" (args' fp) ""
  let hOpenSSL = Prelude.head $ BSC.split ' ' out
  let hTinfoil = final $ hf inp
  pure $ hOpenSSL === hTinfoil
  where
    args' tmpf = ["dgst", "-r"] <> args <> [tmpf]

