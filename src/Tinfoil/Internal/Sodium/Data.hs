{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinfoil.Internal.Sodium.Data (
    SodiumInitStatus(..)
  , AESGCMSupport(..)
  ) where

import           P

data SodiumInitStatus =
    SodiumInitialised
  | SodiumNotInitialised
  deriving (Eq, Show, Enum, Bounded)

data AESGCMSupport =
    AESGCMSupported
  | AESGCMNotSupported
  deriving (Eq, Show, Enum, Bounded)

