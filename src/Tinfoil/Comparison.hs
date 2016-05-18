{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Tinfoil.Comparison(
    safeEq
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)

-- | Constant-time comparison. Will reveal if the length of the inputs differ
-- by exiting early, but will provide no other information.
safeEq :: ByteString -> ByteString -> IO Bool
safeEq a b =
  BSU.unsafeUseAsCStringLen a $ \(aPtr, aLen) ->
    BSU.unsafeUseAsCStringLen b $ \(bPtr, bLen) ->
      cBool <$> tinfoil_const_cmp (castPtr aPtr) (fromIntegral aLen) (castPtr bPtr) (fromIntegral bLen)
  where
    cBool 1 = True
    cBool _ = False

foreign import ccall safe tinfoil_const_cmp
  :: Ptr Word8
  -> CSize
  -> Ptr Word8
  -> CSize
  -> IO Word8
