{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Tinfoil.Signing.Ed25519.Internal (
    genKeyPair
  , maxSigLen
  , pubKeyLen
  , secKeyLen
  , signMessage'
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr (nullPtr)

import           P

import           System.IO (IO)

import           Tinfoil.Data.Key
import           Tinfoil.Data.Signing

-- | Generate a new Ed25519 keypair.
--
-- FIXME: init requirement for thread safety?
genKeyPair :: IO (Maybe' (PublicKey Ed25519, SecretKey Ed25519))
genKeyPair = do
  allocaBytes pubKeyLen $ \pubKeyPtr ->
    allocaBytes secKeyLen $ \secKeyPtr -> do
      sodium_ed25519_keypair (castPtr pubKeyPtr) (castPtr secKeyPtr) >>= \case
        0 -> do
          pk <- BS.packCStringLen (pubKeyPtr, pubKeyLen)
          sk <- BS.packCStringLen (secKeyPtr, secKeyLen)
          pure $ Just' (PKey_Ed25519 pk, SKey_Ed25519 sk)
        _ ->
          -- This can't happen afaict (sodium-0.4.5).
          pure Nothing'

foreign import ccall safe "crypto_sign_ed25519_keypair" sodium_ed25519_keypair
  :: Ptr Word8 -- public key
  -> Ptr Word8 -- secret key
  -> IO CInt

-- | Generate an Ed25519-signed message (signature prepended to message text).
signMessage' :: SecretKey Ed25519 -> ByteString -> Maybe' ByteString
signMessage' (SKey_Ed25519 sk) msg = unsafePerformIO $
  let smLen = maxSigLen + (BS.length msg) in do
  allocaBytes smLen $ \smPtr ->
      -- Key length bounded, so we don't need to pass it explicitly here.
      BS.useAsCStringLen sk $ \(skPtr, _skLen) ->
        BS.useAsCStringLen msg $ \(msgPtr, msgLen) -> do
          r <- sodium_sign_ed25519 (castPtr smPtr)
                                   -- We already know the length, don't need
                                   -- libsodium to tell us.
                                   (castPtr nullPtr)
                                   (castPtr msgPtr)
                                   (fromIntegral msgLen)
                                   (castPtr skPtr)
          case r of
            0 -> fmap Just' $ BS.packCStringLen (smPtr, smLen)
            _ -> pure Nothing'

-- libsodium 0.4.5 (the latest we have available on CentOS 6) doesn't support
-- detached signatures.
foreign import ccall safe "crypto_sign_ed25519" sodium_sign_ed25519
  :: Ptr Word8 -- signed message buffer
  -> Ptr CULong -- signed message length buffer
  -> Ptr Word8 -- message
  -> CSize -- message length
  -> Ptr Word8 -- secret key
  -> IO CInt

-- | 32 with versions of libsodium up to 1.0.10-1.
pubKeyLen :: Int
pubKeyLen = fromIntegral tinfoil_sodium_pubkey_len

foreign import ccall unsafe tinfoil_sodium_pubkey_len
  :: CSize

-- | 64 with versions of libsodium up to 1.0.10-1.
secKeyLen :: Int
secKeyLen = fromIntegral tinfoil_sodium_seckey_len

foreign import ccall unsafe tinfoil_sodium_seckey_len
  :: CSize

-- | 64 with versions of libsodium up to 1.0.10-1.
maxSigLen :: Int
maxSigLen = fromIntegral tinfoil_sodium_sig_len

foreign import ccall unsafe tinfoil_sodium_sig_len
  :: CSize
