{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tinfoil.Arbitrary where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Utf8 (genValidUtf8)

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
  arbitrary = Credential <$> genValidUtf8

-- Fake entropy.
instance Arbitrary Entropy where
  arbitrary = Entropy <$> arbitrary

newtype InvalidCredentialHash =
  InvalidCredentialHash {
    unInvalidCredentialHash :: CredentialHash
  } deriving (Eq, Show)

instance Arbitrary InvalidCredentialHash where
  arbitrary = InvalidCredentialHash <$> 
    ((CredentialHash . T.encodeUtf8) <$> elements muppets)

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

instance Arbitrary KeyedHashFunction where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary SignatureVersion where
  arbitrary = elements [minBound..maxBound]

