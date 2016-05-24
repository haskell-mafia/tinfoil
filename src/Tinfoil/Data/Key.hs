{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinfoil.Data.Key(
    SymmetricKey(..)
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

newtype SymmetricKey =
  SymmetricKey {
    unSymmetricKey :: ByteString
  } deriving (Eq, Generic)

instance NFData SymmetricKey where rnf = genericRnf
