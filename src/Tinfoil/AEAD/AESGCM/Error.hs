{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Tinfoil.AEAD.AESGCM.Error (
    AesGcmError(..)
  ) where

import           P

data AesGcmError =
    MalformedCiphertext
  | InvalidAuthenticationTag
  deriving (Eq, Show)
