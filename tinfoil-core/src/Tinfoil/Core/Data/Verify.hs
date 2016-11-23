{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.Verify(
    Verified(..)
) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

data Verified =
    Verified -- ^ Credential hash is well-formed and credential is correct.
  | NotVerified -- ^ Credential hash is well-formed but credential is not correct.
  | VerificationError -- ^ Credential hash is not well-formed.
  deriving (Eq, Show, Generic)

instance NFData Verified where rnf = genericRnf

