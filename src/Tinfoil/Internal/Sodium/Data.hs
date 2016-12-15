{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinfoil.Internal.Sodium.Data (
    SodiumInitStatus(..)
  ) where

import           P

data SodiumInitStatus =
    SodiumInitialised
  | SodiumNotInitialised
  deriving (Eq, Show, Enum, Bounded)
