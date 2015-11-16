{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Data(
    Entropy(..)
  , Credential(..)
  , CredentialHash(..)
  , CHF(..)
) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

import           P

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

-- | Output of a 'CHF'. Do not ever implement an 'Eq' instance for
-- this type.
newtype CredentialHash =
  CredentialHash {
    unCredentialHash :: ByteString
  } deriving (Show)

newtype Credential =
  Credential {
    unCredential :: Text
  } deriving (Eq, Show)

-- | Credential hashing function - put in a secret and get out a token
-- from which it is computationally infeasible to derive the secret.
--
-- Good KDFs usually make good CHFs.
--
-- Properties:
--  * Uses an underlying cryptographic hash function
--  (collision-resistant). 
--  * Salted with high-quality entropy (to make rainbow tables
--  infeasible).
--  * Slow, for naive brute-force.
--  * High memory requirements, for highly-parallel low-memory
--  processors (GPUs, mining ASICs, et cetera).
data CHF = CHF
  { genHash    :: (Entropy -> Credential -> Maybe CredentialHash)
  , mcfPrefix  :: Text
  }
