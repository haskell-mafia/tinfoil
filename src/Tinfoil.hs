{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil(
  -- * ubiquitous types
    Verified(..)
  -- * safe comparison
  , ConstEq(..)
  , safeEq

  -- * utilities
  , hexEncode
  ) where

import           Tinfoil.Comparison
import           Tinfoil.Data.Verify
import           Tinfoil.Encode
