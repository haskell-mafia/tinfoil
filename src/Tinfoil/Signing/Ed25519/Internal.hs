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

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C
import           Foreign.Marshal.Alloc (allocaBytes)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Tinfoil.Data.KDF
import           Tinfoil.Data.Key

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

-- | Generate an Ed25519-signed message (signature prepended to message text).
signMessage' :: SecretKey Ed25519 -> ByteString -> Maybe' ByteString
signMessage' (SKey_Ed25519 sk) msg = unsafePerformIO $
  let smLen = maxSigLen + (BS.length msg) in do
  allocaBytes smLen $ \smPtr ->
    allocaBytes 8 $ \smlPtr ->
      -- Key length bounded, so we don't need to pass it explicitly here.
      BS.useAsCStringLen sk $ \(skPtr, _skLen) ->
        BS.useAsCStringLen msg $ \(msgPtr, msgLen) -> do
          r <- sodium_sign_ed25519 (castPtr smPtr)
                                   -- We already know the length, but older
                                   -- versions of libsodium don't allow a
                                   -- null pointer to be passed here.
                                   (castPtr smlPtr)
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
  -> Ptr CULong -- signed message length buffer or null
  -> Ptr Word8 -- message
  -> CSize -- message length
  -> Ptr Word8 -- secret key
  -> IO CInt

-- | Verify an Ed25519-signed message (signature prepended to message).
verifyMessage' :: PublicKey Ed25519 -> ByteString -> Verified
verifyMessage' (PKey_Ed25519 pk) sm = unsafePerformIO $ do
  allocaBytes (BS.length sm) $ \msgPtr -> -- wasting a few bytes here but w/e
    allocaBytes 8 $ \mlPtr ->
      -- Key length bounded, so we don't need to pass it explicitly here.
      BS.useAsCStringLen pk $ \(pkPtr, _pkLen) ->
        BS.useAsCStringLen sm $ \(smPtr, smLen) -> do
          r <- sodium_open_ed25519 (castPtr msgPtr)
                                   -- libsodium needs somewhere to write the
                                   -- message length.
                                   (castPtr mlPtr)
                                   (castPtr smPtr)
                                   (fromIntegral smLen)
                                   (castPtr pkPtr)
          -- We ignore the values written to msgPtr and mlPtr, we know those
          -- things already. We pass the length explicitly, so there's no risk
          -- of verifying a smaller message than intended.
          case r of
            0 -> pure Verified
            (-1) -> pure NotVerified
            _ -> pure VerificationError -- impossible


foreign import ccall safe "crypto_sign_ed25519_open" sodium_open_ed25519
  :: Ptr Word8 -- message buffer
  -> Ptr CULong -- message length buffer or null pointer
  -> Ptr Word8 -- signed message
  -> CSize -- signed message length
  -> Ptr Word8 -- public key
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
