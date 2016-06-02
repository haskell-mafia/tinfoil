{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Disorder.Core.Gen (GenSeed(..), genDeterministic)

import           P

import qualified Prelude

import           System.IO
import qualified System.Random as R

import           Test.Tinfoil.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Comparison
import           Tinfoil.Data
import           Tinfoil.Hash
import qualified Tinfoil.KDF.Scrypt as Scrypt
import           Tinfoil.MAC
import           Tinfoil.Random
import qualified Tinfoil.Signing.Ed25519 as Ed25519

generate' :: Gen a -> IO a
generate' = pure . genDeterministic (GenSeed 314159)

bsTriple :: Int -> Int -> Gen (ByteString, ByteString, ByteString)
bsTriple small big = do
  short1 <- vectorOf small arbitrary
  short2 <- vectorOf small arbitrary `suchThat` (/= short1)
  long <- vectorOf big arbitrary
  let big1 = BS.pack $ short2 <> long
  let big2 = BS.copy big1
  pure (BS.pack $ short1 <> long, big1, big2)

genEd25519 :: IO (SecretKey Ed25519, PublicKey Ed25519, Signature Ed25519, ByteString)
genEd25519 = do
  (pk, sk) <- Ed25519.genKeyPair
  msg <- generate' arbitrary
  let sig = fromJust' $ Ed25519.signMessage sk msg
  pure (sk, pk, sig, msg)
  where
    fromJust' Nothing' = Prelude.error "impossible: signing valid message failed"
    fromJust' (Just' x) = x

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
            reportFile = Just "dist/build/tinfoil-bench.html"
          , csvFile = Just "dist/build/tinfoil-bench.csv"
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
  , env (generate' $ bsTriple 5 10000) $ \ ~(h1, h2_1, h2_2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2_1 h2_1)
                           , bench "safe/different" $ nfIO (safeEq h1 h2_1)
                           , bench "unsafe/same" $ nfIO (unsafeEq h2_1 h2_2)
                           , bench "unsafe/different" $ nfIO (unsafeEq h1 h2_1)
                         ]
  , env (generate' $ bsTriple 5 10000) $ \ ~(h1, h2_1, h2_2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2_1 h2_1)
                           , bench "safe/different" $ nfIO (safeEq h1 h2_1)
                           , bench "unsafe/same" $ nfIO (unsafeEq h2_1 h2_2)
                           , bench "unsafe/different" $ nfIO (unsafeEq h1 h2_1)
                           ]
  , env (generate' $ bsTriple 5 10000) $ \ ~(h1, h2_1, h2_2) ->
      bgroup "eqs/large" $ [ bench "safe/same" $ nfIO (safeEq h2_1 h2_1)
                           , bench "safe/different" $ nfIO (safeEq h1 h2_1)
                           , bench "unsafe/same" $ nfIO (unsafeEq h2_1 h2_2)
                           , bench "unsafe/different" $ nfIO (unsafeEq h1 h2_1)
                           ]
  , env (generate' arbitrary) $ \ ~cred ->
      bgroup "kdf/scrypt" $ [ bench "hashCredential/defaultParams" $ nfIO (Scrypt.hashCredential Scrypt.defaultParams cred)
                            , bench "verifyNoCredential/defaultParams" $ nfIO (Scrypt.verifyNoCredential Scrypt.defaultParams cred)
                            ]
  , env (generate' arbitrary) $ \ ~bs ->
      bgroup "hash/SHA256" $ [ bench "hashSHA256" $ nf hashSHA256 bs
                             ]
  , env ((,) <$> generate' arbitrary <*> generate' arbitrary) $ \ ~(sk, bs) ->
      bgroup "mac/hmacSHA256" $ [ bench "hmacSHA256" $ nf (hmacSHA256 sk) bs
                                ]
  , env genEd25519 $ \ ~(sk, pk, sig, msg) ->
      bgroup "signing/ed25519" $ [
          bench "genKeyPair" $ nfIO Ed25519.genKeyPair
        , bench "signMessage" $ nf (Ed25519.signMessage sk) msg
        , bench "verifyMessage" $ nf (Ed25519.verifyMessage pk sig) msg
        ]
  ]
