{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Tinfoil.AEAD.AESGCM.Data where

import           P

import           System.IO

import           Test.QuickCheck hiding ((.&.))
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.AEAD.AESGCM.Data

prop_packInvocationField :: InvocationField -> Property
prop_packInvocationField invoc =
  invoc === (unpackInvocationField $ packInvocationField invoc)

prop_packGcmIv :: GcmIv -> Property
prop_packGcmIv iv =
  (Just' iv) === (unpackGcmIv $ packGcmIv iv)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
