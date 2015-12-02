{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Tinfoil.KDF.Scrypt where

import           Disorder.Core.IO (testIO, withCPUTime)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Tinfoil.Data (Credential(..), Verified(..))
import           Tinfoil.KDF.Scrypt
import           Tinfoil.KDF.Scrypt.Internal
import           Tinfoil.Random (entropy)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary

-- 1.5 seconds in picoseconds
minHashTime :: Integer
minHashTime = 15 * 10^(11 :: Integer)

prop_verifyCredential_valid :: ScryptParams -> UniquePair Credential -> Property
prop_verifyCredential_valid p (UniquePair good bad) = testIO $ do
  ch <- hashCredential p good
  r1 <- verifyCredential ch good
  r2 <- verifyCredential ch bad
  pure $ (r1, r2) === (Just Verified, Just NotVerified)

prop_verifyCredential_timing :: UniquePair Credential -> Property
prop_verifyCredential_timing (UniquePair good bad) = testIO $ do
  ch <- hashCredential defaultParams good
  (t1, r1) <- withCPUTime $ verifyCredential ch good
  (t2, r2) <- withCPUTime $ verifyCredential ch bad
  pure $ (r1, r2, t1 >= minHashTime, t2 >= minHashTime) === (Just Verified, Just NotVerified, True, True)


prop_verifyCredential_invalid :: Credential -> InvalidCredentialHash -> Property
prop_verifyCredential_invalid c (InvalidCredentialHash ch) = testIO $ do
  r <- verifyCredential ch c
  pure $ r === Nothing

-- Run twice and check the times to make sure we're not memoizing the result.
prop_verifyNoCredential :: Property
prop_verifyNoCredential = testIO $ do
  let a = verifyNoCredential defaultParams
  (t1, r1) <- withCPUTime a
  (t2, r2) <- withCPUTime a
  pure $ (r1, r2, t1 > minHashTime, t2 > minHashTime) === (Just NotVerified, Just NotVerified, True, True)
  

prop_scrypt :: ScryptParams -> Credential -> Property
prop_scrypt p c = testIO $ do
  e <- entropy 32
  h1 <- scrypt p e c
  h2 <- scrypt p e c
  pure $ h1 === h2

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 } )
