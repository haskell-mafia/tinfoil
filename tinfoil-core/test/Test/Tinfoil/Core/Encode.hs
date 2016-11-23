{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Core.Encode where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Core.Encode

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping_hex :: ByteString -> Property
prop_tripping_hex bs =
  let bl = BS.length bs in
  tripping hexEncode (hexDecode bl) bs

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
