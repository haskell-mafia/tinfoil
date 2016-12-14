{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
This module contains constraints on message lengths and message counts
which apply to AES256-GCM as implemented in tinfoil. The constraints
are mostly the same as the recommendations in
<dx.doi.org/10.6028/NIST.SP.800-38D NIST Special Publication 800-38D>;
there are some additional constraints imposed by IV construction in our case.
-}
module Tinfoil.AEAD.AESGCM.Constraints (
    Blocks(..)
  , Bytes(..)
  , Messages(..)

  , blocksBytes
  , maxCleartextLength
  , maxMessageCount
  , maxTotalData
) where

import           P

-- |
-- Number of blocks. A block is 128 bits.
newtype Blocks = Blocks {
    unBlocks :: Integer
  } deriving (Eq, Show, Ord, Enum, Num)

-- |
-- Number of bytes. Assumes that bytes are equivalent to octets; this is
-- true on all supported architectures (i.e., x86_64).
newtype Bytes = Bytes {
    unBytes :: Integer
  } deriving (Eq, Show, Ord, Enum, Num)

-- |
-- Count (number) of messages.
newtype Messages = Messages {
    unMessages :: Integer
  } deriving (Eq, Show, Ord, Enum, Num)

-- |
-- Converts blocks to bytes (octets).
blocksBytes :: Blocks -> Bytes
blocksBytes (Blocks n) =
  Bytes $ (n * 128) `div` 8

-- |
-- Upper bound on blocks encrypted with one (key, IV) pair (i.e., one
-- message). In bytes, this is around 64GiB.
--
-- GCM's internal counter is 32 bits, so we can't encrypt more than
-- @2^32@ blocks; block size is @2^7@ bits, so we have @2^39@ bits worth of
-- counter. We lose one block due to the GCM counter skipping 0, and another
-- to encrypt the @GHASH@ with the first block in the keystream.
--
-- So in blocks, we have a limit of @(2^32 - 2)@. GHC will not constant-fold
-- this, so we use the literal.
--
-- <dx.doi.org/10.6028/NIST.SP.800-38D NIST SP 800-38D>
maxCleartextLength :: Blocks
maxCleartextLength =
  Blocks 4294967294

-- |
-- Upper bound on number of messages encrypted with the same key. This is
-- due to the length of the counter part of the invocation field of the IV,
-- @2^32@ bits. GHC will not constant-fold this, so we use the literal.
maxMessageCount :: Messages
maxMessageCount =
  Messages 4294967296

-- |
-- Maximum total number of blocks which can be authed or encrypted using a
-- single key (2^64). This limit is for the sum of all cleartext input blocks to
-- AES-GCM, not including IV blocks or key blocks but including both cleartext
-- for encryption and associated data for authentication. GHC will not
-- constant-fold this, so we use the literal.
--
-- <dx.doi.org/10.6028/NIST.SP.800-38D NIST SP 800-38D>
maxTotalData :: Blocks
maxTotalData =
  Blocks 18446744073709551616
