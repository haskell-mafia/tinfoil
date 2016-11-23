{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil (
  -- * ubiquitous types
    Verified(..)
  -- * safe comparison
  , ConstEq(..)
  , safeEq

  -- * utilities
  , hexEncode
  ) where

import           Tinfoil.Core.Comparison
import           Tinfoil.Core.Data.Verify
import           Tinfoil.Core.Encode
