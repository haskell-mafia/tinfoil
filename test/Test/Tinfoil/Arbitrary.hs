{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tinfoil.Arbitrary where

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets, viruses)

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data
import           Tinfoil.Random
import           Tinfoil.KDF.Scrypt.Internal

instance Arbitrary a => Arbitrary (NonEmpty a)
  where
    arbitrary =
      NE.fromList <$> listOf1 arbitrary

credentialLength :: Gen Int
credentialLength = choose (0, 512)

excludedChars :: Gen [Char]
excludedChars = arbitrary `suchThat` (/= (NE.toList credentialCharSet))

instance Arbitrary Credential where
  arbitrary = do
    n <- credentialLength
    bs <- fmap BS.pack . vectorOf n $ choose (0, 255)
    pure $ Credential bs

-- Fake entropy.
instance Arbitrary Entropy where
  arbitrary = Entropy <$> arbitrary

genInvalidCredentialHash :: Gen CredentialHash
genInvalidCredentialHash =
  ((CredentialHash . T.encodeUtf8) <$> elements muppets)

-- valid MCF prefix, invalid hash part
genWellFormedMCFHash :: Gen MCFHash
genWellFormedMCFHash = do
  p <- arbitrary
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

newtype DrawBits =
  DrawBits {
    unDrawBits :: [Bool]
  } deriving (Eq, Show)

instance Arbitrary DrawBits where
  arbitrary = do
    n <- choose (0, 63)
    DrawBits <$> vectorOf n arbitrary

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

instance Arbitrary ScryptParams where
  arbitrary = ScryptParams <$> logN <*> r <*> p
    where
      logN :: Gen Int
      logN = choose (1, 20)
      
      r :: Gen Int
      r = choose (1, 10)
      
      p :: Gen Int
      p = choose (1, 20)

-- Unsafe, test code only.
instance Eq CredentialHash where
  (CredentialHash a) == (CredentialHash b) = a == b

-- Unsafe, test code only.
instance Eq MCFHash where
  (MCFHash a) == (MCFHash b) = a == b

instance Arbitrary KeyedHashFunction where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary MCFPrefix where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary SymmetricKey where
  arbitrary = do
    n <- choose (0, 100)
    xs <- vectorOf n $ choose (0, 255)
    pure . SymmetricKey $ BS.pack xs

-- Able to be passed to `openssl dgst [...] -macopt hexkey:`
genOpenSSLSymmetricKey :: Gen SymmetricKey
genOpenSSLSymmetricKey = do
  n <- choose (1, 100)
  xs <- vectorOf n $ choose (0, 255)
  pure . SymmetricKey $ BS.pack xs

-- Unsafe, test code only.
instance Show SymmetricKey where
  show (SymmetricKey x) = "SymmetricKey " <> show x

-- Unsafe, test code only.
instance Eq MAC where
  (MAC a) == (MAC b) = a == b

instance Arbitrary SignatureAlgorithm where
  arbitrary = elements [minBound..maxBound]
