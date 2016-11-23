{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.Key(
    SymmetricKey(..)
  , parseSymmetricKey
  , renderSymmetricKey
  , symmetricKeyLength
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           GHC.Show (appPrec, appPrec1)

import           Tinfoil.Core.Encode

-- | A cryptographically-random string of bits. A 'SymmetricKey' is 32
-- bytes wide, which is for an intended 128-bit security level
-- assuming the possibility of birthday attacks.
newtype SymmetricKey =
  SymmetricKey {
    unSymmetricKey :: ByteString
  } deriving (Eq, Generic)

instance NFData SymmetricKey where
  rnf = genericRnf

-- | We use fixed valid Haskell tokens here to help prevent accidents such as
-- inadvertantly logging secret keys.
instance Show SymmetricKey where
  showsPrec p _ =
    showParen (p > appPrec) $
      showString "SymmetricKey " . showsPrec appPrec1 ("redacted" :: ByteString)

symmetricKeyLength :: Int
symmetricKeyLength = 32

renderSymmetricKey :: SymmetricKey -> Text
renderSymmetricKey = hexEncode . unSymmetricKey

parseSymmetricKey :: Text -> Maybe' SymmetricKey
parseSymmetricKey t = SymmetricKey <$> hexDecode symmetricKeyLength t

