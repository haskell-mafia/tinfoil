{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.KDF.Scrypt(
    ScryptParams
  , defaultParams
  , hashCredential
  , paramsUpToDate
  , scryptMCFPrefix
  , verifyCredential
  , verifyNoCredential
) where

import qualified Data.ByteString as BS

import           P

import           System.IO

import           Tinfoil.Data (Credential(..), CredentialHash(..), Entropy(..))
import           Tinfoil.Data (Verified(..), NeedsRehash(..))
import           Tinfoil.KDF.Scrypt.Internal
import           Tinfoil.KDF.Common
import           Tinfoil.Random (entropy)

scryptMCFPrefix :: Text
scryptMCFPrefix = "$scrypt0$"

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
  Just (p, e, h) -> do
    h' <- scrypt p e c
    r <- h' `safeEq` h
    if r
      then pure Verified
      else pure NotVerified
  Nothing        -> pure VerificationError

hashCredential :: ScryptParams -> Credential -> IO CredentialHash
hashCredential params cred = do
  s <- salt
  (combine params s) <$> scrypt params s cred

paramsUpToDate :: CredentialHash -> Maybe NeedsRehash
paramsUpToDate h = do
  (ps, _, _) <- separate h
  if ps == defaultParams
    then pure UpToDate
    else pure NeedsRehash
    
