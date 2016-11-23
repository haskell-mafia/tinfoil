{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Core.Random.Internal(
    explodeBS
  , readBitsLE
  , dropMS
  , segmentsOf
) where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

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
