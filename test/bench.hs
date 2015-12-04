{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P
import           Prelude (($!))

import           System.IO
import qualified System.Random as R

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

-- non-CSPRNG, just a performance baseline.
stdRandom :: Int -> IO ByteString
stdRandom n = BS.pack <$> R.getStdRandom (genBytes n [])
  where
    genBytes 0 l g = (l, g)
    genBytes n' l g =
      let (value, nextG) = R.random g
      in genBytes (n' - 1) (value : l) nextG

unsafeEq :: ByteString -> ByteString -> IO Bool
unsafeEq a b = pure $! a == b

tinfoilBench :: [Benchmark] -> IO ()
tinfoilBench = defaultMainWith cfg
  where
    cfg = defaultConfig {
            reportFile = Just "dist/bench.html"
          }

main :: IO ()
main = tinfoilBench [
    bgroup "randomCredential" $
      [ bench "1" $ nfIO (randomCredential [] 1)
      , bench "1000" $ nfIO (randomCredential [] 1000)
      , bench "100000" $ nfIO (randomCredential [] 100000)
      ]
  , bgroup "entropy" $
      [ bench "Tinfoil.Random.entropy/1" $ nfIO (entropy 1)
      , bench "Tinfoil.Random.entropy/1000" $ nfIO (entropy 1000)
      , bench "Tinfoil.Random.entropy/100000" $ nfIO (entropy 100000)
      , bench "System.Random.StdRandom/1" $ nfIO (stdRandom 1)
      , bench "System.Random.StdRandom/1000" $ nfIO (stdRandom 1000)
      , bench "System.Random.StdRandom/100000" $ nfIO (stdRandom 100000)
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
  ]
