{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Core.KDF.Scrypt(
    ScryptParams
  , defaultParams
  , hashCredential
  , paramsUpToDate
  , verifyCredential
  , verifyNoCredential
) where

import qualified Data.ByteString as BS

import           P

import           System.IO

import           Tinfoil.Core.Comparison
import           Tinfoil.Core.Data (Credential(..), CredentialHash(..), Entropy(..))
import           Tinfoil.Core.Data (Verified(..), NeedsRehash(..))
import           Tinfoil.Core.KDF.Scrypt.Internal
import           Tinfoil.Core.Random (entropy)

-- |
-- Nontrivial but reasonable memory usage, and a runtime of at
-- least 2sec. Memory usage should be 128Nr = 64MiB/hash per
-- <https://tarsnap.com/scrypt/scrypt.pdf the paper>.
defaultParams :: ScryptParams
defaultParams = scryptParams 16 8 12 -- N = 2^16, r = 8, p = 12

salt :: IO Entropy
salt = entropy 32

verifyNoCredential :: ScryptParams -> Credential -> IO Verified
verifyNoCredential p c = do
  h <- scrypt p e c
  void $ h `safeEq` ""
  pure NotVerified
  where
    e = Entropy $ BS.replicate 32 0x00

verifyCredential :: CredentialHash -> Credential -> IO Verified
verifyCredential ch c = case separate ch of
  Just' (p, e, h) -> do
    h' <- scrypt p e c
    r <- h' `safeEq` h
    if r
      then pure Verified
      else pure NotVerified
  Nothing'        -> pure VerificationError

hashCredential :: ScryptParams -> Credential -> IO CredentialHash
hashCredential params cred = do
  s <- salt
  (combine params s) <$> scrypt params s cred

-- | Check that the parameters in the hash match the provided set of parameters.
-- Nothing' if the hash is invalid.
paramsUpToDate :: ScryptParams -> CredentialHash -> Maybe' NeedsRehash
paramsUpToDate p h = do
  (ps, _, _) <- separate h
  if ps == p
    then pure UpToDate
    else pure NeedsRehash
