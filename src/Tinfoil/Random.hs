{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Random(
    randomCredential
  , credentialCharSet
  , drawOnce
  , entropy
  , randomWord32
) where

import           Control.Exception (bracket)

import           Data.Bits (shiftL, (.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List ((\\))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word (Word32)

import           Foreign (allocaBytes)
import           Foreign.Ptr (plusPtr, castPtr)

import           P

import qualified Prelude

import qualified System.Posix as Posix

import           System.IO

import           Tinfoil.Data
import           Tinfoil.Random.Internal

-- | Reads the specified number of bytes from from /dev/urandom.
--
-- On OS X this calls a 160-bit Yarrow generator and will block until
-- the entropy pool has been initialised (should never happen); on
-- Linux it is a sha1-based generator and will not block regardless of
-- pool state.
--
-- FIXME: is it worth checking uname and failing on non-linux/darwin here?
entropy :: Int -> IO Entropy
entropy n =
  fmap Entropy $ bracket
    (Posix.openFd "/dev/urandom" Posix.ReadOnly Nothing Posix.defaultFileFlags)
    (Posix.closeFd)
    (entropy' n)

entropy' :: Int -> Posix.Fd -> IO ByteString
entropy' n fd =
  allocaBytes n $ \buf ->
    loop buf $ fromIntegral n
  where
    loop buf 0 =
      BS.packCStringLen (castPtr buf, fromIntegral n)
    loop buf toread =
      let
        buf' = plusPtr buf $ n - (fromIntegral toread)
      in
      -- fdReadBuf handles the ret < 0 case for us.
      Posix.fdReadBuf fd buf' toread >>= \ret -> case ret of
        0 ->
          Prelude.error "/dev/urandom returned EOF; this should never happen."
        nread ->
          loop buf $ toread - nread
  
-- | Generate a random 4-byte word. If treated as unsigned integers,
-- values will be uniformly distributed over [0, 2^32-1].
randomWord32 :: IO Word32
randomWord32 =
  let
    pack byte acc = (acc `shiftL` 8) .|. (fromIntegral byte)
  in
  entropy 4 >>= \(Entropy x) ->
    pure . foldr' pack 0 $ BS.unpack x

-- | Generate a password of a given length using the printable ASCII 
-- characters (excluding tabs and newlines). This implementation is pretty 
-- slow and can be made faster if necessary.
--
-- For platforms with restrictions on characters allowed in passwords, you
-- can pass in a list of characters to exclude from 'credentialCharSet'.
randomCredential :: [Char] -> Int -> IO (Maybe Credential)
randomCredential excluded n = case charset of
  [] -> pure Nothing
  cs -> (Just . Credential . T.encodeUtf8 . T.pack) <$> replicateM n (drawOnce $ NE.fromList cs)
  where
    charset = (NE.toList credentialCharSet) \\ excluded 

-- | All the printable ASCII characters except newlines and tabs.
credentialCharSet :: NonEmpty Char
credentialCharSet = NE.fromList [' '..'~']

-- | Draw one element from the input list uniformly at random.
drawOnce :: NonEmpty a -> IO a
drawOnce as = do
    r <- (readBitsLE . discard . explodeBS . unEntropy) <$> entropy entropyBytes
    if r < n
      then pure (as NE.!! r)
      else drawOnce as
  where
    -- Don't need these, so drop them to reduce the number of times we need to
    -- draw.
    extraBits = (entropyBytes * 8) - entropyBits

    -- Required entropy, rounded up to nearest byte.
    entropyBytes :: Int
    entropyBytes = ceiling ((fromIntegral entropyBits) / (8.0 :: Double))

    -- Required entropy, rounded up to nearest bit.
    entropyBits :: Int
    entropyBits = ceiling . logBase (2.0 :: Double) $ fromIntegral n

    n = NE.length as

    discard = dropMS extraBits
