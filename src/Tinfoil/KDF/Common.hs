{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Tinfoil.KDF.Common(
    hashEq
) where

import           P

import           System.IO (IO)

import           Tinfoil.Comparison
import           Tinfoil.Data

-- | This is in IO because hash comparison is not a pure function -
-- the timing is one of the outputs.
hashEq :: CredentialHash -> CredentialHash -> IO Bool
hashEq (CredentialHash a) (CredentialHash b) = safeEq a b
