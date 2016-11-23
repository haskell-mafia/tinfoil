{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Tinfoil.Sign.Ed25519.Internal where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Core.Data
import           Tinfoil.Sign.Ed25519.Internal

prop_genKeyPair_len :: Property
prop_genKeyPair_len = testIO $ do
  (PKey_Ed25519 pk, SKey_Ed25519 sk) <- genKeyPair
  pure $ (BS.length pk, BS.length sk) === (pubKeyLen, secKeyLen)

prop_genKeyPair :: Property
prop_genKeyPair = testIO $ do
  (pk1, sk1) <- genKeyPair
  (pk2, sk2) <- genKeyPair
  pure $ (pk1 == pk2, sk1 == sk2) === (False, False)

-- Check the signed-message construction works how we think it does.
prop_signMessage' :: ByteString -> Property
prop_signMessage' msg = testIO $ do
  (_pk, sk) <- genKeyPair
  case signMessage' sk msg of
    Nothing' ->
      pure . failWith $ "Unexpected failure signing: " <> T.pack (show msg)
    Just' sm ->
      let msg' = BS.drop maxSigLen sm in
      pure $ msg === msg'

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
