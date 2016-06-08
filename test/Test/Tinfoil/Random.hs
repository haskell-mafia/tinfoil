{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Random where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List (zip)
import           Data.Word (Word8)

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Random.Internal


import           Test.Tinfoil.Arbitrary
import           Test.Tinfoil.Gen
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_explodeBS :: ByteString -> Property
prop_explodeBS = tripping explodeBS implodeBS
  where
    implodeBS :: [Bool] -> Maybe ByteString
    implodeBS bts
      | (length bts) `mod` 8 == 0 = Just . BS.pack . fmap implodeWord $ segmentsOf 8 bts
      | otherwise                = Nothing

    implodeWord :: [Bool] -> Word8
    implodeWord bts = foldr copyBit zeroBits $ zip bts [0..7]

    copyBit (True, i) acc = setBit acc i
    copyBit (False, _) acc = acc

prop_readExploded :: ByteString -> Property
prop_readExploded bs =
  let a = readBitsLE $ explodeBS bs
      b = readBSLE bs
  in a === b
  where
    readBSLE = BS.foldr' pack 0

    pack byte acc = (acc `shiftL` 8) .|. fromIntegral byte

prop_readExploded_ones :: Property
prop_readExploded_ones = forAll drawOnes32 $ \bts ->
  let v = floor . logBase (2.0 :: Double) . fromIntegral $ readBitsLE bts
      w = length bts - 1 in
  v === w

prop_readExploded_zeroes :: Property
prop_readExploded_zeroes = forAll drawZeroes32 $ \bts ->
  (readBitsLE bts) === 0

prop_segmentsOf :: Property
prop_segmentsOf = forAll splitList $ \(n, xs) ->
    concat (segmentsOf n xs) === xs
  where
    splitList :: Gen (Int, [Int])
    splitList = do
      n <- choose (10, 1000)
      m <- choose (5, 100)
      xs <- vectorOf (n*m) arbitrary
      pure (n, xs)

prop_dropMS :: DrawBits -> Property
prop_dropMS (DrawBits xs) = forAll (choose (0, (length xs) - 1)) $ \n ->
  let ys = drop n xs in
  readBitsLE xs >= readBitsLE ys

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
