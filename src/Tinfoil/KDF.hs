{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.KDF(
    defaultScrypt
  , hash
  , kdfFor
  , needsRehash
  , verify
  , verifyNoCredential
) where

import           P

import           System.IO (IO)

import           Tinfoil.Data
import qualified Tinfoil.KDF.Scrypt as Scrypt

defaultScrypt :: KDF
defaultScrypt =
  KDF
    (Scrypt.hashCredential Scrypt.defaultParams)
    Scrypt.verifyCredential
    (Scrypt.verifyNoCredential Scrypt.defaultParams)
    Scrypt0
    (Scrypt.paramsUpToDate Scrypt.defaultParams)

kdfFor :: MCFPrefix -> KDF
kdfFor Scrypt0 = defaultScrypt

hash :: MCFPrefix -> Credential -> IO MCFHash
hash mp c = do
  fmap (packMCFHash mp) $ (kdfGenHash kdf) c
  where
    kdf = kdfFor mp

verify :: MCFHash -> Credential -> IO Verified
verify mh c =
  maybe' (pure VerificationError) (uncurry verify') $ unpackMCFHash mh
  where
    verify' mcf ch =
      let kdf = kdfFor mcf in
      (kdfVerifyCredential kdf) ch c

verifyNoCredential :: MCFPrefix -> Credential -> IO Verified
verifyNoCredential mp c =
  (kdfVerifyNoCredential kdf) $ c
  where
    kdf = kdfFor mp

needsRehash :: MCFHash -> Maybe' NeedsRehash
needsRehash mh = do
  (p, h) <- unpackMCFHash mh
  (kdfUpToDate (kdfFor p)) h
