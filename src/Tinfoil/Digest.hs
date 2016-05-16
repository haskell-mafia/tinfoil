{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinfoil.Digest(
    hexDigest
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as T

import           P

hexDigest :: ByteString -> Text
hexDigest = T.decodeUtf8 . Base16.encode
