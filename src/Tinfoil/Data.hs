{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data(
    Entropy(..)
  , Credential(..)
  , CredentialHash(..)
  , Verified(..)
  , NeedsRehash(..)
  , Verification(..)
  , KDF(..)
) where

import           Control.DeepSeq (NFData)

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

import           GHC.Generics (Generic)

import           P

import           System.IO

-- | Sufficiently-random data. In almost all cases[0], use
-- <https://hackage.haskell.org/package/entropy-0.3.7/docs/System-Entropy.html System.Entropy.getEntropy>.
-- 
-- [0] For completeness, possible exceptions arise when generating key
-- material on Linux extremely early in the boot process (before the
-- network is up).
newtype Entropy =
  Entropy {
    unEntropy :: ByteString
  } deriving (Eq, Show)

-- | Output of a 'KDF'. Do not ever implement an 'Eq' instance for
-- this type.
newtype CredentialHash =
  CredentialHash {
    unCredentialHash :: ByteString
  } deriving (Show, Generic)

instance NFData CredentialHash

newtype Credential =
  Credential {
    unCredential :: Text
  } deriving (Eq, Show, Generic)

instance NFData Credential

data Verified =
    Verified
  | NotVerified
  deriving (Eq, Show, Generic)

instance NFData Verified

data NeedsRehash =
    NeedsRehash
  | UpToDate
  deriving (Eq, Show)

data Verification = Verification Verified NeedsRehash
  deriving (Eq, Show)

-- | Key derivation function - put in a secret and get out a token
-- from which it is computationally infeasible to derive the secret, which
-- is suitable either for use as a cryptographic key or as a credential hash.
--
-- Properties:
--  * Uses an underlying cryptographic hash function or other pseudo-random
--  function with good collision resistance and diffusion.
--  * Salted with high-quality entropy (to make rainbow tables
--  infeasible).
--  * Slow, for naive brute-force.
--  * High memory requirements, for highly-parallel low-memory
--  processors (GPUs, mining ASICs, et cetera).
data KDF = KDF
  { genHash        :: (Credential -> IO CredentialHash)
  , hashCredential :: (Credential -> CredentialHash -> IO Verified)
  , verifyNoHash   :: IO Verified
  , mcfPrefix      :: Text
  }
