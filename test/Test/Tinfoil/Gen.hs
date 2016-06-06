{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tinfoil.Gen where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets, viruses)

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data
import           Tinfoil.Random

credentialLength :: Gen Int
credentialLength = choose (0, 512)

excludedChars :: Gen [Char]
excludedChars = arbitrary `suchThat` (/= (NE.toList credentialCharSet))

genInvalidCredentialHash :: Gen CredentialHash
genInvalidCredentialHash =
  ((CredentialHash . T.encodeUtf8) <$> elements muppets)

genMCFPrefix :: Gen MCFPrefix
genMCFPrefix =
  elements [minBound..maxBound]

-- valid MCF prefix, invalid hash part
genWellFormedMCFHash :: Gen MCFHash
genWellFormedMCFHash = do
  p <- genMCFPrefix
  h <- genInvalidCredentialHash
  pure $ packMCFHash p h

genInvalidMCFHash :: Gen MCFHash
genInvalidMCFHash = oneof [
    (MCFHash . T.encodeUtf8) <$> elements muppets
  , invalidPrefix
  ]
  where
    invalidPrefix = do
      p <- elements muppets
      h <- elements viruses
      pure . MCFHash . T.encodeUtf8 $ T.concat [
          "$"
        , p
        , "$"
        , h
        ]

-- For tests involving floating-point computations (e.g., logs) which don't
-- retain sufficient precision close to 64 bits.
drawOnes32 :: Gen [Bool]
drawOnes32 = do
  n <- choose (1, 32)
  vectorOf n $ pure True

drawZeroes32 :: Gen [Bool]
drawZeroes32 = do
  n <- choose (1, 32)
  vectorOf n $ pure False

-- Able to be passed to `openssl dgst [...] -macopt hexkey:`
genOpenSSLSymmetricKey :: Gen SymmetricKey
genOpenSSLSymmetricKey = do
  n <- choose (1, 100)
  xs <- vectorOf n $ choose (0, 255)
  pure . SymmetricKey $ BS.pack xs
