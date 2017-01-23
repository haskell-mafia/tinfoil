{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Tinfoil.AEAD.AESGCM.Foreign (
    aes256GcmEncrypt
  , aes256GcmDecrypt
  , aes256GcmTagLength
  ) where

import qualified Data.ByteString as BS
import           Data.Word (Word8)

import           Foreign.C.Types (CInt(..), CULLong(..), CSize(..))
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.Storable (peek)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.AEAD.AESGCM.Error
import           Tinfoil.Data.Key
import           Tinfoil.Internal.Sodium

foreign import ccall safe "tinfoil_sodium_aead_tag_len" tinfoil_sodium_aead_tag_len
  :: CSize

aes256GcmTagLength :: Int
aes256GcmTagLength =
  fromIntegral tinfoil_sodium_aead_tag_len

foreign import ccall safe "crypto_aead_aes256gcm_encrypt" sodium_aes256gcm_encrypt
  :: Ptr Word8 -- unsigned char *c // ciphertext
  -> Ptr CULLong -- unsigned long long *clen_p // ciphertext length
  -> Ptr Word8 -- const unsigned char *m // cleartext
  -> CULLong -- unsigned long long mlen // cleartext length
  -> Ptr Word8 -- const unsigned char *ad // associated data
  -> CULLong -- unsigned long long adlen // associated data length
  -> Ptr Word8 -- const unsigned char *nsec // this should always be NULL
  -> Ptr Word8 -- const unsigned char *npub // nonce
  -> Ptr Word8 -- const unsigned char *k // key
  -> IO CInt

aes256GcmEncrypt
  :: SodiumInitMarker
  -> Cleartext
  -> AssociatedData
  -> SymmetricKey
  -> GcmIv
  -> AuthenticatedCiphertext
aes256GcmEncrypt _mark (Cleartext m) (AssociatedData ad) (SymmetricKey k) iv =
  let
    iv' = packGcmIv iv
  in
  unsafePerformIO . BS.useAsCStringLen m $ \(msg_p, msg_len) ->
    BS.useAsCStringLen ad $ \(ad_p, ad_len) ->
      BS.useAsCString k $ \k_p ->
        BS.useAsCString iv' $ \iv_p ->
          allocaBytes (msg_len + aes256GcmTagLength) $ \ciphertext_p ->
            alloca $ \ciphertext_len_p -> do
              -- In spite of the type, this can't actually fail.
              void $ sodium_aes256gcm_encrypt
                (castPtr ciphertext_p)
                (castPtr ciphertext_len_p)
                (castPtr msg_p)
                (fromIntegral msg_len)
                (castPtr ad_p)
                (fromIntegral ad_len)
                (castPtr nullPtr)
                (castPtr iv_p)
                (castPtr k_p)
              ciphertext_len <- peek ciphertext_len_p
              ciphertext <- BS.packCStringLen (ciphertext_p, ciphertext_len)
              pure $ AuthenticatedCiphertext ciphertext

foreign import ccall safe "crypto_aead_aes256gcm_decrypt" sodium_aes256gcm_decrypt
  :: Ptr Word8 -- unsigned char *c // cleartext
  -> Ptr CULLong -- unsigned long long *clen_p // cleartext length
  -> Ptr Word8 -- const unsigned char *nsec // this should always be NULL
  -> Ptr Word8 -- const unsigned char *m // ciphertext
  -> CULLong -- unsigned long long mlen // ciphertext length
  -> Ptr Word8 -- const unsigned char *ad // associated data
  -> CULLong -- unsigned long long adlen // associated data length
  -> Ptr Word8 -- const unsigned char *npub // nonce
  -> Ptr Word8 -- const unsigned char *k // key
  -> IO CInt

aes256GcmDecrypt
  :: SodiumInitMarker
  -> AuthenticatedCiphertext
  -> AssociatedData
  -> SymmetricKey
  -> GcmIv
  -> Either AesGcmError Cleartext
aes256GcmDecrypt _mark (AuthenticatedCiphertext ct) (AssociatedData ad) (SymmetricKey k) iv
  | BS.length ct < aes256GcmTagLength =
    Left MalformedCiphertext
  | otherwise =
    let
      iv' = packGcmIv iv
    in
    unsafePerformIO . BS.useAsCStringLen ct $ \(ct_p, ct_len) ->
      BS.useAsCStringLen ad $ \(ad_p, ad_len) ->
        BS.useAsCString k $ \k_p ->
          BS.useAsCString iv' $ \iv_p ->
            allocaBytes ct_len $ \cleartext_p ->
              alloca $ \cleartext_len_p -> do
                r <- sodium_aes256gcm_decrypt
                       (castPtr cleartext_p)
                       (castPtr cleartext_len_p)
                       (castPtr nullPtr)
                       (castPtr ct_p)
                       (fromIntegral ct_len)
                       (castPtr ad_p)
                       (fromIntegral ad_len)
                       (castPtr iv_p)
                       (castPtr k_p)
                case r of
                  0 -> do
                    cleartext_len <- peek cleartext_len_p
                    cleartext <- BS.packCStringLen (cleartext_p, cleartext_len)
                    pure . Right $ Cleartext cleartext
                  _ ->
                    pure $ Left InvalidAuthenticationTag
