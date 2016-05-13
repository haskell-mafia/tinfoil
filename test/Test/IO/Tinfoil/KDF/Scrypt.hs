{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Tinfoil.KDF.Scrypt where

import           Disorder.Core.IO (testIO, withCPUTime)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary
import           Test.Tinfoil.KDF.Scrypt.TestVectors

import           Tinfoil.Data (Credential(..), Verified(..))
import           Tinfoil.Data (NeedsRehash(..))
import           Tinfoil.KDF.Scrypt
import           Tinfoil.KDF.Scrypt.Internal
import           Tinfoil.Random (entropy)

-- 1.5 seconds in picoseconds
minHashTime :: Integer
minHashTime = 15 * 10^(11 :: Integer)

prop_verifyCredential_valid :: ScryptParams -> UniquePair Credential -> Property
prop_verifyCredential_valid p (UniquePair good bad) = testIO $ do
  ch <- hashCredential p good
  r1 <- verifyCredential ch good
  r2 <- verifyCredential ch bad
  pure $ (r1, r2) === (Verified, NotVerified)

prop_verifyCredential_timing :: UniquePair Credential -> Property
prop_verifyCredential_timing (UniquePair good bad) = testIO $ do
  ch <- hashCredential defaultParams good
  (t1, r1) <- withCPUTime $ verifyCredential ch good
  (t2, r2) <- withCPUTime $ verifyCredential ch bad
  pure $ (r1, r2, t1 >= minHashTime, t2 >= minHashTime) === (Verified, NotVerified, True, True)


prop_verifyCredential_invalid :: Credential -> Property
prop_verifyCredential_invalid c =
  forAll genInvalidCredentialHash $ \ch -> testIO $ do
    r <- verifyCredential ch c
    pure $ r === VerificationError

-- Run twice and check the times to make sure we're not memoizing the result.
prop_verifyNoCredential :: Credential -> Property
prop_verifyNoCredential c = testIO $ do
  let a = verifyNoCredential defaultParams c
  (t1, r1) <- withCPUTime a
  (t2, r2) <- withCPUTime a
  pure $ (r1, r2, t1 > minHashTime, t2 > minHashTime) === (NotVerified, NotVerified, True, True)
  
prop_scrypt :: ScryptParams -> Credential -> Property
prop_scrypt p c = testIO $ do
  e <- entropy 32
  h1 <- scrypt p e c
  h2 <- scrypt p e c
  pure $ h1 === h2

-- Verify that the test vectors from the scrypt paper pass.
prop_testVector :: TestVector -> Property
prop_testVector (TestVector c s p h) = testIO $ do
  h' <- scrypt p s c
  pure $ h' === h

prop_paramsUpToDate :: UniquePair ScryptParams -> Credential -> Property
prop_paramsUpToDate (UniquePair p1 p2) c = testIO $ do
  h1 <- hashCredential p1 c
  h2 <- hashCredential p2 c
  let r1 = paramsUpToDate p1 h1
  let r2 = paramsUpToDate p1 h2
  pure $ (r1, r2) === (Just' UpToDate, Just' NeedsRehash)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 } )
