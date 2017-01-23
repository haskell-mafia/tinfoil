{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Tinfoil.AEAD.AESGCM.Foreign where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Tinfoil.Arbitrary ()
import           Test.Tinfoil.Gen

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.AEAD.AESGCM.Error
import           Tinfoil.AEAD.AESGCM.Foreign
import           Tinfoil.Data.Key
import           Tinfoil.Internal.Sodium

import           X.Control.Monad.Trans.Either (runEitherT)

testSodium :: (SodiumInitMarker -> Property) -> Property
testSodium p =
  testIO $ (runEitherT initialiseSodium) >>= \x -> pure $ case x of
    Right mark -> p mark
    Left err -> failWith . T.pack $ show err

prop_aes256Gcm_encrypt :: AssociatedData -> SymmetricKey -> GcmIv -> Property
prop_aes256Gcm_encrypt ad sk iv = forAll (Cleartext <$> genByteStringWithin (16, 512)) $ \clear ->
  testSodium $ \m ->
    let
      ct = aes256GcmEncrypt m clear ad sk iv
    in
    not (BS.null $ unCleartext clear) ==>
      BS.isInfixOf (unCleartext clear) (unAuthenticatedCiphertext ct) === False

prop_aes256Gcm_decrypt :: ByteString -> AssociatedData -> SymmetricKey -> GcmIv -> Property
prop_aes256Gcm_decrypt ct ad sk iv =
  testSodium $ \m ->
    let
      clear = aes256GcmDecrypt m (AuthenticatedCiphertext ct) ad sk iv
    in
    isLeft clear === True

prop_aes256Gcm_decrypt_short :: AssociatedData -> SymmetricKey -> GcmIv -> Property
prop_aes256Gcm_decrypt_short ad sk iv = forAll (genByteStringWithin (0, aes256GcmTagLength - 1)) $ \ct ->
  testSodium $ \m ->
    let
      clear = aes256GcmDecrypt m (AuthenticatedCiphertext ct) ad sk iv
    in
    clear === Left MalformedCiphertext

prop_aes256Gcm :: Cleartext -> AssociatedData -> SymmetricKey -> GcmIv -> Property
prop_aes256Gcm clear ad sk iv =
  testSodium $ \m ->
    tripping (\msg -> aes256GcmEncrypt m msg ad sk iv) (\ct -> aes256GcmDecrypt m ct ad sk iv) $ clear


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
