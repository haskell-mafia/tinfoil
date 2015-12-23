{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.KDF(
    safeEq
  , hashEq
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P
import           Prelude (($!))

import           System.IO (IO)

import           Tinfoil.Data

-- | Constant-time comparison. Not at all optimised, and accordingly
-- around three orders of magnitude slower than the 'Eq' instance
-- (which is on the order of a microsecond for password-hash-sized
-- 'ByteString's).
safeEq :: ByteString -> ByteString -> IO Bool
safeEq a b = pure $
  (&&) (la == lb) $! (match (buffered a) (buffered b) True)
  where
    buffered bs = bs <> BS.replicate (maxLen - (BS.length bs)) 0

    maxLen = fromIntegral $ max la lb

    (la, lb) = (BS.length a, BS.length b)

    match as bs acc
      | BS.null as || BS.null bs =
          acc
      | otherwise                =
          match (BS.tail as) (BS.tail bs)
              ((&&) acc $! (BS.head as == BS.head bs))

-- | This is in IO because hash comparison is not a pure function -
-- the timing is one of the outputs.
hashEq :: CredentialHash -> CredentialHash -> IO Bool
hashEq (CredentialHash a) (CredentialHash b) = safeEq a b
