{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.KDF.Scrypt(
    defaultParams
  , hashCredential
  , verifyCredential
  , verifyNoCredential
) where

import           P

import           System.IO

import           Tinfoil.Data (Credential(..), CredentialHash(..), Entropy(..))
import           Tinfoil.Data (Verified(..))
import           Tinfoil.KDF.Scrypt.Internal
import           Tinfoil.KDF
import           Tinfoil.Random (entropy)

-- |
-- Nontrivial but reasonable memory usage, and a runtime of at
-- least 2sec. Memory usage should be 128Nr = 64MiB/hash per
-- <https://tarsnap.com/scrypt/scrypt.pdf the paper>.
defaultParams :: ScryptParams
defaultParams = scryptParams 16 8 12 -- N = 2^16, r = 8, p = 12

salt :: IO Entropy
salt = entropy 32

verifyNoCredential :: ScryptParams -> IO (Maybe Verified)
verifyNoCredential p = do
  e <- salt
  h <- scrypt p e (Credential "")
  void $ h `safeEq` ""
  pure $ Just NotVerified

verifyCredential :: CredentialHash -> Credential -> IO (Maybe Verified)
verifyCredential ch c = case separate ch of
  Just (p, e, h) -> do
    void salt -- timing consistency
    h' <- scrypt p e c
    r <- h' `safeEq` h
    if r
      then pure $ Just Verified
      else pure $ Just NotVerified
  Nothing        -> pure Nothing

hashCredential :: ScryptParams -> Credential -> IO CredentialHash
hashCredential params cred = do
  s <- salt
  (combine params s) <$> scrypt params s cred
