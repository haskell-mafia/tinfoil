{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Tinfoil.Hash.TestVectors where

import           Data.ByteString (ByteString)

import           P

import           System.IO

import           Tinfoil.Data.Hash
import           Tinfoil.Digest
import           Tinfoil.Hash

import           Test.QuickCheck

testTestVector :: (ByteString -> Hash) -> ByteString -> Text -> Property
testTestVector f inVec outVec =
  let r = hexDigest . unHash $ f inVec in
  once $ r === outVec

-- SHA2 test vectors from the
-- <https://www.cosic.esat.kuleuven.be/nessie/testvectors/ NESSIE project>.

prop_sha256TestVec1 =
  testTestVector
    hashSHA256
    ""
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

prop_sha256TestVec2 =
  testTestVector
    hashSHA256
    "a"
    "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"

prop_sha256TestVec3 =
  testTestVector
    hashSHA256
    "abc"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

prop_sha256TestVec4 =
  testTestVector
    hashSHA256
    "message digest"
    "f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650"

prop_sha256TestVec5 =
  testTestVector
    hashSHA256
    "abcdefghijklmnopqrstuvwxyz"
    "71c480df93d6ae2f1efad1447c66c9525e316218cf51fc8d9ed832f2daf18b73"

prop_sha256TestVec6 =
  testTestVector
    hashSHA256
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"

prop_sha256TestVec7 =
  testTestVector
    hashSHA256
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    "db4bfcbd4da0cd85a60c3c37d3fbd8805c77f15fc6b1fdfe614ee0a7c8fdb4c0"

prop_sha256TestVec8 =
  testTestVector
    hashSHA256
    "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "f371bc4a311f2b009eef952dd83ca80e2b60026c8e935592d0f9c308453c813e"

return []
tests :: IO Bool
tests = $quickCheckAll
