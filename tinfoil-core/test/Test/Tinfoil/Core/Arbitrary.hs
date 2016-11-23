{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tinfoil.Core.Arbitrary where

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Core.Gen

import           Tinfoil.Core.Data
import           Tinfoil.Core.KDF.Scrypt.Internal

instance Arbitrary a => Arbitrary (NonEmpty a)
  where
    arbitrary =
      NE.fromList <$> listOf1 arbitrary

instance Arbitrary Credential where
  arbitrary = do
    n <- credentialLength
    bs <- fmap BS.pack . vectorOf n $ choose (0, 255)
    pure $ Credential bs

-- Fake entropy.
instance Arbitrary Entropy where
  arbitrary = Entropy <$> arbitrary

newtype DrawBits =
  DrawBits {
    unDrawBits :: [Bool]
  } deriving (Eq, Show)

instance Arbitrary DrawBits where
  arbitrary = do
    n <- choose (0, 63)
    DrawBits <$> vectorOf n arbitrary

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
  arbitrary = genMCFPrefix

instance Arbitrary SymmetricKey where
  arbitrary = do
    xs <- vectorOf 32 $ choose (0, 255)
    pure . SymmetricKey $ BS.pack xs

-- Unsafe, test code only.
instance Eq MAC where
  (MAC a) == (MAC b) = a == b

instance Arbitrary SignatureAlgorithm where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary HashFunction where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary MAC where
  arbitrary = genUBytes MAC 32
