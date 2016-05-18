{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Hash(
    Hash(..)
) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

-- | Binary representation of a hash.
newtype Hash =
  Hash {
    unHash :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData Hash where rnf = genericRnf
