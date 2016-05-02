{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.KDF(
    defaultScrypt
) where

import           Tinfoil.Data
import qualified Tinfoil.KDF.Scrypt as Scrypt

defaultScrypt :: KDF
defaultScrypt =
  KDF
    (Scrypt.hashCredential Scrypt.defaultParams)
    Scrypt.verifyCredential
    (Scrypt.verifyNoCredential Scrypt.defaultParams)
    Scrypt.scryptMCFPrefix
