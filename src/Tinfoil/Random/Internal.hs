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

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           System.IO (IOMode(..), IO)
import           System.IO (withFile)

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
-- This function incurs some overhead due to GHC.IO buffering; if this
-- becomes significant (e.g., key generation), using read(3) directly
-- would make small reads significantly faster.
--
-- FIXME: is it worth checking uname and failing on non-linux/darwin here?
urandom :: Int -> IO ByteString
urandom n =
  withFile "/dev/urandom" ReadMode (\h -> BS.hGet h n)
