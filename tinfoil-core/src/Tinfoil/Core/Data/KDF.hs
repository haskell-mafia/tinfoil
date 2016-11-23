{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.KDF(
    Credential(..)
  , CredentialHash(..)
  , KDF(..)
  , MCFHash(..)
  , MCFPrefix(..)
  , NeedsRehash(..)
  , Verification(..)
  , packMCFHash
  , parseMCFPrefix
  , renderMCFPrefix
  , renderMCFHash
  , unpackMCFHash
) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word (Word8)

import           GHC.Generics (Generic)

import           P

import           System.IO

import           Tinfoil.Core.Data.Verify

-- | Output of a 'KDF'. Do not ever implement an 'Eq' instance for
-- this type.
newtype CredentialHash =
  CredentialHash {
    unCredentialHash :: ByteString
  } deriving (Show, Generic)

instance NFData CredentialHash where rnf = genericRnf

-- | Credential hash wrapped up with an MCF prefix.
newtype MCFHash =
  MCFHash {
    unMCFHash :: ByteString
  } deriving (Show, Generic)

instance NFData MCFHash where rnf = genericRnf

renderMCFHash :: MCFHash -> ByteString
renderMCFHash = unMCFHash

newtype Credential =
  Credential {
    unCredential :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData Credential where rnf = genericRnf

data NeedsRehash =
    NeedsRehash
  | UpToDate
  deriving (Eq, Show, Generic)

instance NFData NeedsRehash where rnf = genericRnf

data Verification = Verification !Verified !NeedsRehash
  deriving (Eq, Show, Generic)

instance NFData Verification where rnf = genericRnf

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
  { kdfGenHash :: (Credential -> IO CredentialHash)
  , kdfVerifyCredential :: (CredentialHash -> Credential -> IO Verified)
  , kdfVerifyNoCredential :: (Credential -> IO Verified)
  , kdfMcfPrefix :: MCFPrefix
  , kdfUpToDate :: CredentialHash -> Maybe' NeedsRehash
  }

-- | Non-standardized modular crypt format string. Uniquely identifies (from
-- tinfoil's perspective) a KDF algorithm.
data MCFPrefix =
    Scrypt0
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData MCFPrefix where rnf = genericRnf

renderMCFPrefix :: MCFPrefix -> ByteString
renderMCFPrefix Scrypt0 = "scrypt0"

parseMCFPrefix :: ByteString -> Maybe' MCFPrefix
parseMCFPrefix "scrypt0" = pure Scrypt0
parseMCFPrefix _ = Nothing'

unpackMCFHash :: MCFHash -> Maybe' (MCFPrefix, CredentialHash)
unpackMCFHash (MCFHash bs) = do
  (p, h) <- splitMCF
  p' <- parseMCFPrefix p
  pure (p', CredentialHash h)
  where
    splitMCF = case BS.split mcfDelimiter bs of
      ("":x:ys) -> pure (x, BS.intercalate (BS.singleton mcfDelimiter) ys)
      _ -> Nothing'

packMCFHash :: MCFPrefix -> CredentialHash -> MCFHash
packMCFHash p h = MCFHash $ BS.concat [
    BS.singleton mcfDelimiter
  , renderMCFPrefix p
  , BS.singleton mcfDelimiter
  , unCredentialHash h
  ]

mcfDelimiter :: Word8
mcfDelimiter = 0x24
