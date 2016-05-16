{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Tinfoil.KDF where

import           Disorder.Core.IO (testIO, withCPUTime)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary

import           Tinfoil.Data
import           Tinfoil.KDF
import qualified Tinfoil.KDF.Scrypt as Scrypt

-- 1.5 seconds in picoseconds
minHashTime :: Integer
minHashTime = 15 * 10^(11 :: Integer)

prop_verify_valid :: MCFPrefix -> UniquePair Credential -> Property
prop_verify_valid mp (UniquePair good bad) = testIO $ do
  ch <- hash mp good
  r1 <- verify ch good
  r2 <- verify ch bad
  pure $ (r1, r2) === (Verified, NotVerified)

-- Results shouldn't vary before/after param updates.
prop_verify_changing :: MCFPrefix -> UniquePair Credential -> Property
prop_verify_changing mp (UniquePair good bad) = forAll (arbitrary `suchThat` (not . (== Scrypt.defaultParams))) $ \scp -> testIO $ do
  ch1 <- fmap (packMCFHash mp) $ Scrypt.hashCredential scp good
  let utd1 = needsRehash ch1
  ch2 <- hash mp good
  let utd2 = needsRehash ch2
  r1 <- verify ch1 good
  r2 <- verify ch2 good
  r3 <- verify ch1 bad
  r4 <- verify ch2 bad
  pure $ (r1, r2, r3, r4, utd1, utd2) === (Verified, Verified, NotVerified, NotVerified, Just' NeedsRehash, Just' UpToDate)

prop_verify_timing :: MCFPrefix -> UniquePair Credential -> Property
prop_verify_timing mp (UniquePair good bad) = testIO $ do
  ch <- hash mp good
  (t1, r1) <- withCPUTime $ verify ch good
  (t2, r2) <- withCPUTime $ verify ch bad
  pure $ (r1, r2, t1 >= minHashTime, t2 >= minHashTime) === (Verified, NotVerified, True, True)

prop_verify_invalid :: Credential -> Property
prop_verify_invalid c =
  forAll ((,) <$> genWellFormedMCFHash <*> genInvalidMCFHash) $ \(wfh, ih) -> testIO $ do
    r1 <- verify wfh c
    r2 <- verify ih c
    pure $ (r1, r2) === (VerificationError, VerificationError)

-- Run twice and check the times to make sure we're not memoizing the result.
prop_verifyNoCredential :: MCFPrefix -> Credential -> Property
prop_verifyNoCredential mp c = testIO $ do
  let a = verifyNoCredential mp c
  (t1, r1) <- withCPUTime a
  (t2, r2) <- withCPUTime a
  pure $ (r1, r2, t1 > minHashTime, t2 > minHashTime) === (NotVerified, NotVerified, True, True)

prop_needsRehash :: MCFPrefix -> Credential -> Property
prop_needsRehash mp c = testIO $ do
  h <- hash mp c
  pure $ (needsRehash h) === (Just' UpToDate)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 } )
