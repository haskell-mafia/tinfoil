{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tinfoil.AEAD.AESGCM.Iv (
    IvError(..)

  , newInvocationField
  , incrementInvocationField
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.AEAD.AESGCM.Data
import           Tinfoil.Random

data IvError =
    InvocationCountOverflow

newInvocationField :: IO InvocationField
newInvocationField =
  InvocationField <$> (RandomField <$> randomWord32) <*> (pure $ InvocationCount 0)

incrementInvocationField :: InvocationField -> Either IvError InvocationField
incrementInvocationField (InvocationField r (InvocationCount n)) =
  let
    n' = n + 1
  in
  case n' > ((2 :: Integer)^(32 :: Integer) - 1) of
    True -> Left InvocationCountOverflow
    False -> Right . InvocationField r $ InvocationCount n'
