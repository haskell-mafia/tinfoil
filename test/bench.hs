{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Criterion.Main

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Random
import           Tinfoil.KDF

hashPair :: Int -> Int -> Gen (ByteString, ByteString)
hashPair small big = do
  short1 <- vectorOf small arbitrary
  short2 <- vectorOf small arbitrary `suchThat` (/= short1)
  long <- vectorOf big arbitrary
  pure (BS.pack $ short1 <> long, BS.pack $ short2 <> long)

unsafeEq :: (ByteString, ByteString) -> Bool
unsafeEq = uncurry (==)

main :: IO ()
main = defaultMain [
    bgroup "randomCredential" $
      [ bench "1" $ nfIO (randomCredential [] 1)
      , bench "1000" $ nfIO (randomCredential [] 1000)
      , bench "100000" $ nfIO (randomCredential [] 100000)
      ]
  , env (generate $ hashPair 5 1000) $ \ ~(h1, h2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2 h2)
                           , bench "safe/different" $ nfIO (safeEq h1 h2)
                           , bench "unsafe/same" $ nf unsafeEq (h2, h2)
                           , bench "unsafe/different" $ nf unsafeEq (h1, h2)
                           ]
  ]
