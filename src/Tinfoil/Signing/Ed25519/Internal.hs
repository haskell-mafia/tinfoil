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
  , verifyMessage'
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Foreign (Ptr, Word8, castPtr, nullPtr)
import           Foreign.C (CSize(..), CULLong(..), CInt(..))
import           Foreign.Marshal.Alloc (allocaBytes)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Tinfoil.Data.Key
import           Tinfoil.Data.Verify

-- | Generate a new Ed25519 keypair.
--
-- This function is thread-safe.
--
-- Note: the above assumes libsodium's default CSPRNG implementation
-- is used (/dev/urandom on older Linuxes, getrandom(2) on newer Linuxes),
-- when calling `crypto_sign_ed25519_keypair`, which is true as used by
-- tinfoil.
genKeyPair :: IO (PublicKey Ed25519, SecretKey Ed25519)
genKeyPair = do
  allocaBytes pubKeyLen $ \pubKeyPtr ->
    allocaBytes secKeyLen $ \secKeyPtr -> do
      -- In spite of the type, this can't actually fail.
      void $ sodium_ed25519_keypair (castPtr pubKeyPtr) (castPtr secKeyPtr)
      pk <- BS.packCStringLen (pubKeyPtr, pubKeyLen)
      sk <- BS.packCStringLen (secKeyPtr, secKeyLen)
      pure (PKey_Ed25519 pk, SKey_Ed25519 sk)

foreign import ccall safe "crypto_sign_ed25519_keypair" sodium_ed25519_keypair
  :: Ptr Word8 -- public key
  -> Ptr Word8 -- secret key
  -> IO CInt

-- | Generate an Ed25519 signature of a message.
signMessage' :: SecretKey Ed25519 -> ByteString -> Maybe' ByteString
signMessage' (SKey_Ed25519 sk) msg = unsafePerformIO $
  let sigLen = maxSigLen in do
  allocaBytes sigLen $ \sigPtr ->
    BS.useAsCStringLen sk $ \(skPtr, _skLen) ->
      BS.useAsCStringLen msg $ \(msgPtr, msgLen) -> do
        r <- sodium_sign_ed25519_detached
               (castPtr sigPtr)
               nullPtr -- We already know the length.
               (castPtr msgPtr)
               (fromIntegral msgLen)
               (castPtr skPtr)
        case r of
          0 -> fmap Just' $ BS.packCStringLen (sigPtr, sigLen)
          _ -> pure Nothing'

foreign import ccall safe "crypto_sign_ed25519_detached" sodium_sign_ed25519_detached
  :: Ptr Word8 -- signature buffer (unsigned char *sig)
  -> Ptr CULLong -- signature length (unsigned long long *siglen_p)
  -> Ptr Word8 -- message (const unsigned char *m)
  -> CULLong -- message length (unsigned long long mlen)
  -> Ptr Word8 -- secret key (unsigned char *sk)
  -> IO CInt

-- | Verify an Ed25519 detached signature of a message.
verifyMessage' :: PublicKey Ed25519 -> ByteString -> ByteString -> Verified
verifyMessage' (PKey_Ed25519 pk) sig msg = unsafePerformIO $
  BS.useAsCStringLen sig $ \(sigPtr, _sigLen) ->
    BS.useAsCStringLen msg $ \(msgPtr, msgLen) ->
      BS.useAsCStringLen pk $ \(pkPtr, _pkLen) -> do
        r <- sodium_verify_ed25519_detached
          (castPtr sigPtr)
          (castPtr msgPtr)
          (fromIntegral msgLen)
          (castPtr pkPtr)
        case r of
          0 -> pure Verified
          (-1) -> pure NotVerified
          _ -> pure VerificationError -- impossible


foreign import ccall safe "crypto_sign_ed25519_verify_detached" sodium_verify_ed25519_detached
  :: Ptr Word8 -- signature buffer (const unsigned char *sig)
  -> Ptr Word8 -- message buffer (const unsigned char *m)
  -> CULLong -- message length (unsigned long long mlen)
  -> Ptr Word8 -- public key (const unsigned char *pk)
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
