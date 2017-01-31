{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tinfoil.AEAD.AESGCM.Iv where

import           Disorder.Core (failWith)
import           Disorder.Core.Property ((=/=))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Tinfoil.Arbitrary ()

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.AEAD.AESGCM.Iv

prop_incrementInvocationField :: InvocationField -> Property
prop_incrementInvocationField f =
  (incrementInvocationField f) =/= (Right f)

prop_incrementInvocationField_overflow :: InvocationField -> Property
prop_incrementInvocationField_overflow f =
  let
    f' = incrementInvocationField f
  in
  isRight f' ==> case f' of
    Left _ ->
      failWith "Unexpected left"
    Right (InvocationField _ (InvocationCount x)) ->
      x =/= 0


prop_incrementGcmIv :: GcmIv -> Property
prop_incrementGcmIv iv =
  (incrementGcmIv iv) =/= (Right iv)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
