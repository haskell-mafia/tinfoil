{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Random.Internal(
    explodeBS
  , readBitsLE
  , dropMS
  , segmentsOf
  , urandom
) where

import           Control.Exception (bracket)

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Foreign (allocaBytes)
import           Foreign.Ptr (plusPtr, castPtr)

import           P

import qualified Prelude

import qualified System.Posix as Posix

import           System.IO (IO)

segmentsOf :: Int -> [a] -> [[a]]
segmentsOf _ [] = []
segmentsOf n xs = as : segmentsOf n bs
  where
    (as, bs) = splitAt n xs

dropMS :: Int -> [Bool] -> [Bool]
dropMS n = reverse . drop n . reverse

readBitsLE :: [Bool] -> Int
readBitsLE = foldr' pack 0
  where
    pack bt acc = (acc `shiftL` 1) .|. fromBool bt

    fromBool True = 1
    fromBool False = 0

explodeBS :: ByteString -> [Bool]
explodeBS = concatMap (\w -> testBit w <$> [0..7]) . BS.unpack

-- |
-- Read entropy from /dev/urandom.
--
-- On OS X this calls a 160-bit Yarrow generator and will block until
-- the entropy pool has been initialised (should never happen); on
-- Linux it is a sha1-based generator and will not block regardless of
-- pool state.
--
-- FIXME: is it worth checking uname and failing on non-linux/darwin here?
urandom :: Int -> IO ByteString
urandom n =
  bracket
    (Posix.openFd "/dev/urandom" Posix.ReadOnly Nothing Posix.defaultFileFlags)
    (Posix.closeFd)
    (urandom' n)

urandom' :: Int -> Posix.Fd -> IO ByteString
urandom' n fd =
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
  
