{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Tinfoil.AEAD.AESGCM.Data where

import qualified Data.Binary.Get as B
import           Data.Bits (shiftR, (.&.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
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

unpackGcmIv :: ByteString -> Maybe GcmIv
unpackGcmIv bs =
  let
    bs' = BSL.fromChunks $ pure bs
    r = flip B.runGetOrFail bs' $ do
          w1 <- B.getWord32le
          w2 <- B.getWord64le
          pure (w1, w2)
  in
  case r of
    Left _ -> Nothing
    Right (_, _, (w1, w2)) ->
      let
        invoc = unpackInvocationField w2
        fixed = FixedField w1
      in
      pure $ GcmIv fixed invoc

prop_packInvocationField :: InvocationField -> Property
prop_packInvocationField invoc =
  invoc === (unpackInvocationField $ packInvocationField invoc)

prop_packGcmIv :: GcmIv -> Property
prop_packGcmIv iv =
  (Just iv) === (unpackGcmIv $ packGcmIv iv)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
