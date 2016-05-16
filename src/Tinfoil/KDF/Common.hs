{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Tinfoil.KDF.Common(
    safeEq
  , hashEq
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)

import           Tinfoil.Data

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

-- | This is in IO because hash comparison is not a pure function -
-- the timing is one of the outputs.
hashEq :: CredentialHash -> CredentialHash -> IO Bool
hashEq (CredentialHash a) (CredentialHash b) = safeEq a b
