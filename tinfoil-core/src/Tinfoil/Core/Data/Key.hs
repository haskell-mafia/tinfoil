{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Tinfoil.Core.Data.Key(
    Ed25519
  , PublicKey(..)
  , SecretKey(..)
  , SymmetricKey(..)
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

data Ed25519

data PublicKey a where
  PKey_Ed25519 :: ByteString -> PublicKey Ed25519

instance Eq (PublicKey a) where
  (PKey_Ed25519 x) == (PKey_Ed25519 y) = x == y

instance NFData (PublicKey a) where
  rnf (PKey_Ed25519 x) = rnf x

data SecretKey a where
  SKey_Ed25519 :: ByteString -> SecretKey Ed25519

instance Eq (SecretKey a) where
  (SKey_Ed25519 x) == (SKey_Ed25519 y) = x == y

instance NFData (SecretKey a) where
  rnf (SKey_Ed25519 x) = rnf x

instance Show (SecretKey a) where
  showsPrec p (SKey_Ed25519 _) =
    showParen (p > appPrec) $
      showString "SKey_Ed25519 " . showsPrec appPrec1 ("redacted" :: ByteString)
