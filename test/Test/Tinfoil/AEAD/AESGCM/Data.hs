{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Tinfoil.AEAD.AESGCM.Data where

import           Data.Bits (shiftR, (.&.))
import           Data.Word (Word64)

import           P

import           System.IO

import           Test.QuickCheck hiding ((.&.))
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.AEAD.AESGCM.Data

unpackInvocationField :: Word64 -> InvocationField
unpackInvocationField w =
  let
    rf = RandomField . fromIntegral $ w `shiftR` 32
    ic = InvocationCount . fromIntegral $ w .&. 0x00000000ffffffff
  in
  InvocationField rf ic

prop_packInvocationField :: InvocationField -> Property
prop_packInvocationField invoc =
  invoc === (unpackInvocationField $ packInvocationField invoc)
  

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
