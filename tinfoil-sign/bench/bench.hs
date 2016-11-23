{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Criterion.Main
import           Criterion.Types

import           Disorder.Core.Gen (GenSeed(..), genDeterministic)

import           P

import qualified Prelude

import           System.IO
import qualified System.Random as R

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Sign.Data
import qualified Tinfoil.Sign.Ed25519 as Ed25519

generate' :: Gen a -> IO a
generate' = pure . genDeterministic (GenSeed 314159)

genEd25519 :: IO (SecretKey Ed25519, PublicKey Ed25519, Signature Ed25519, ByteString)
genEd25519 = do
  (pk, sk) <- Ed25519.genKeyPair
  msg <- generate' arbitrary
  let sig = fromJust' $ Ed25519.signMessage sk msg
  pure (sk, pk, sig, msg)
  where
    fromJust' Nothing' = Prelude.error "impossible: signing valid message failed"
    fromJust' (Just' x) = x

tinfoilBench :: [Benchmark] -> IO ()
tinfoilBench = defaultMainWith cfg
  where
    cfg = defaultConfig {
            reportFile = Just "dist/build/tinfoil-sign-bench.html"
          , csvFile = Just "dist/build/tinfoil-sign-bench.csv"
          }

main :: IO ()
main = tinfoilBench [
    env genEd25519 $ \ ~(sk, pk, sig, msg) ->
      bgroup "signing/ed25519" $ [
          bench "genKeyPair" $ nfIO Ed25519.genKeyPair
        , bench "signMessage" $ nf (Ed25519.signMessage sk) msg
        , bench "verifyMessage" $ nf (Ed25519.verifyMessage pk sig) msg
        ]
  ]
