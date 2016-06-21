{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Encode(
    hexEncode
  , hexDecode
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as T

import           P

hexEncode :: ByteString -> Text
hexEncode = T.decodeUtf8 . Base16.encode

-- | Parse a hex-encoded bytestring of a given length (number of bytes
-- in the data, not its hexadecimal encoding).
hexDecode :: Int -> Text -> Maybe' ByteString
hexDecode len t = case Base16.decode (T.encodeUtf8 t) of
  (x, "") -> if BS.length x == len
               then Just' x
               else Nothing'
  _ -> Nothing'
