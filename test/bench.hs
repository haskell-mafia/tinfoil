{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (formatTime)

import           P
import           Prelude (($!))

import           System.IO
import           System.Locale (defaultTimeLocale)

import           Test.Tinfoil.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Random
import           Tinfoil.KDF
import qualified Tinfoil.KDF.Scrypt as Scrypt

bsTriple :: Int -> Int -> Gen (ByteString, ByteString, ByteString)
bsTriple small big = do
  short1 <- vectorOf small arbitrary
  short2 <- vectorOf small arbitrary `suchThat` (/= short1)
  long <- vectorOf big arbitrary
  let big1 = BS.pack $ short2 <> long
  let big2 = BS.copy big1
  pure (BS.pack $ short1 <> long, big1, big2)

unsafeEq :: ByteString -> ByteString -> IO Bool
unsafeEq a b = pure $! a == b

tinfoilBench :: IO ([Benchmark] -> IO ())
tinfoilBench = do
  t <- getCurrentTime
  let cfg = defaultConfig {
              reportFile = Just $
                "dist/bench-" <> formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S%z" t <> ".html"
            }
  pure (defaultMainWith cfg)

main :: IO ()
main = tinfoilBench >>= (\bench' -> bench' [
    bgroup "randomCredential" $
      [ bench "1" $ nfIO (randomCredential [] 1)
      , bench "1000" $ nfIO (randomCredential [] 1000)
      , bench "100000" $ nfIO (randomCredential [] 100000)
      ]
  , env (generate $ bsTriple 5 10000) $ \ ~(h1, h2_1, h2_2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2_1 h2_1)
                           , bench "safe/different" $ nfIO (safeEq h1 h2_1)
                           , bench "unsafe/same" $ nfIO (unsafeEq h2_1 h2_2)
                           , bench "unsafe/different" $ nfIO (unsafeEq h1 h2_1)
                         ]
  , env (generate $ bsTriple 5 10000) $ \ ~(h1, h2_1, h2_2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2_1 h2_1)
                           , bench "safe/different" $ nfIO (safeEq h1 h2_1)
                           , bench "unsafe/same" $ nfIO (unsafeEq h2_1 h2_2)
                           , bench "unsafe/different" $ nfIO (unsafeEq h1 h2_1)
                           ]
  , env (generate $ bsTriple 5 10000) $ \ ~(h1, h2_1, h2_2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2_1 h2_1)
                           , bench "safe/different" $ nfIO (safeEq h1 h2_1)
                           , bench "unsafe/same" $ nfIO (unsafeEq h2_1 h2_2)
                           , bench "unsafe/different" $ nfIO (unsafeEq h1 h2_1)
                           ]
  , env (generate arbitrary) $ \ ~cred ->
      bgroup "kdf/scrypt" $ [ bench "hashCredential/defaultParams" $ nfIO (Scrypt.hashCredential Scrypt.defaultParams cred)
                            , bench "verifyNoCredential/defaultParams" $ nfIO (Scrypt.verifyNoCredential Scrypt.defaultParams)
                            ]
  ])
