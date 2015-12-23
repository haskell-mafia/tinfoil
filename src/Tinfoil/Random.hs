{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Random(
    randomCredential
  , credentialCharSet
  , drawOnce
  , entropy
) where

import           Data.List ((\\))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           P

import qualified System.Entropy as E
import           System.IO

import           Tinfoil.Data
import           Tinfoil.Random.Internal

-- | Reads the specified number of bytes from from /dev/urandom.
entropy :: Int -> IO Entropy
entropy = fmap Entropy . E.getEntropy

-- | Generate a password of a given length using the printable ASCII 
-- characters (excluding tabs and newlines). This implementation is pretty 
-- slow and can be made faster if necessary.
--
-- For platforms with restrictions on characters allowed in passwords, you
-- can pass in a list of characters to exclude from 'credentialCharSet'.
randomCredential :: [Char] -> Int -> IO (Maybe Credential)
randomCredential excluded n = case charset of
  [] -> pure Nothing
  cs -> (Just . Credential . T.pack) <$> replicateM n (drawOnce $ NE.fromList cs)
  where
    charset = (NE.toList credentialCharSet) \\ excluded 

-- | All the printable ASCII characters except newlines and tabs.
credentialCharSet :: NonEmpty Char
credentialCharSet = NE.fromList [' '..'~']

-- | Draw one element from the input list uniformly at random.
drawOnce :: NonEmpty a -> IO a
drawOnce as = do
    r <- (readBitsLE . discard . explodeBS) <$> E.getEntropy entropyBytes
    if r < n
      then pure (as NE.!! r)
      else drawOnce as
  where
    -- Don't need these, so drop them to reduce the number of times we need to
    -- draw.
    extraBits = (entropyBytes * 8) - entropyBits

    -- Required entropy, rounded up to nearest byte.
    entropyBytes :: Int
    entropyBytes = ceiling ((fromIntegral entropyBits) / (8.0 :: Double))

    -- Required entropy, rounded up to nearest bit.
    entropyBits :: Int
    entropyBits = ceiling . logBase (2.0 :: Double) $ fromIntegral n

    n = NE.length as

    discard = dropMS extraBits
