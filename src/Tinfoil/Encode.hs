{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Encode(
    hexEncode
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as T

import           P

hexEncode :: ByteString -> Text
hexEncode = T.decodeUtf8 . Base16.encode
