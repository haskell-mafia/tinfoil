{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- This module is based on the implementation from Falko Peters' 
-- 'scrypt' package and used under the terms of the BSD3 license.

module Tinfoil.KDF.Scrypt.Internal(
    ScryptParams(..)
  , scryptParams
  , scrypt
  , encodeScryptParams
  , decodeScryptParams
  , combine
  , separate
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as TR

import           Foreign (Ptr, Word8, Word32, Word64, allocaBytes, castPtr)
import           Foreign.C

import           P

import           Prelude (String)

import           System.IO

import           Tinfoil.Data

data ScryptParams =
  ScryptParams {
      scryptLogN :: Int
    , scryptR    :: Int
    , scryptP    :: Int
  } deriving (Eq, Show)

encodeScryptParams :: ScryptParams -> ByteString
encodeScryptParams (ScryptParams logN r p) = T.encodeUtf8 $ T.intercalate "|" [
    T.pack $ show logN
  , T.pack $ show r
  , T.pack $ show p
  ]

decodeScryptParams :: ByteString -> Maybe ScryptParams
decodeScryptParams bs = case BS.split (fromIntegral $ ord '|') bs of
  [logN', r', p'] -> do
    logN <- maybeRead . TR.decimal $ T.decodeUtf8 logN'
    r <- maybeRead . TR.decimal $ T.decodeUtf8 r'
    p <- maybeRead . TR.decimal $ T.decodeUtf8 p'
    pure $ ScryptParams logN r p
  _               -> Nothing
  where
    maybeRead :: Either String (Int, Text) -> Maybe Int
    maybeRead (Left _)        = Nothing
    maybeRead (Right (x, "")) = Just x
    maybeRead (Right (_, _))  = Nothing

scryptParams :: Int -> Int -> Int -> ScryptParams
scryptParams logN r p =
  ScryptParams logN r p

combine :: ScryptParams -> Entropy -> ByteString -> CredentialHash
combine params (Entropy salt) passHash =
    CredentialHash . BS.intercalate "|" $
          encodeScryptParams params
        : [Base64.encode salt, Base64.encode passHash]

separate :: CredentialHash -> Maybe (ScryptParams, Entropy, ByteString)
separate = go . BS.split (fromIntegral $ ord '|') . unCredentialHash
  where
    go [logN', r', p', salt', hash'] = do
        [salt, hash] <- mapM decodeBase64 [salt', hash']

        params <- decodeScryptParams $ BS.intercalate "|" [logN', r', p']
        pure (params, Entropy salt, hash)
    go _         = Nothing
    decodeBase64 = either (const Nothing) Just . Base64.decode

-- This implementation originally from the `scrypt` package; modified to 
-- run in IO.
scrypt :: ScryptParams -> Entropy -> Credential -> IO ByteString
scrypt (ScryptParams logN r p) (Entropy salt) (Credential pass) =
  let pass'  = T.encodeUtf8 pass 
      bufLen = 64 :: Int in
  BS.useAsCStringLen salt $ \(saltPtr, saltLen) ->
  BS.useAsCStringLen pass' $ \(passPtr, passLen) ->
  allocaBytes (fromIntegral bufLen) $ \bufPtr -> do
    throwErrnoIfMinus1_ "crypto_scrypt" $ crypto_scrypt
      (castPtr passPtr) (fromIntegral passLen)
      (castPtr saltPtr) (fromIntegral saltLen)
      (2^logN) (fromIntegral r) (fromIntegral p)
      bufPtr (fromIntegral bufLen)
    BS.packCStringLen (castPtr bufPtr, fromIntegral bufLen)

foreign import ccall safe crypto_scrypt
    :: Ptr Word8 -> CSize         -- password
    -> Ptr Word8 -> CSize         -- salt
    -> Word64 -> Word32 -> Word32 -- N, r, p
    -> Ptr Word8 -> CSize         -- result buffer
    -> IO CInt
