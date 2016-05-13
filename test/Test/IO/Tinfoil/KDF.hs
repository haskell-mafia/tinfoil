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

import           Tinfoil.Data (Credential(..), Verified(..), MCFPrefix(..))
import           Tinfoil.KDF

-- 1.5 seconds in picoseconds
minHashTime :: Integer
minHashTime = 15 * 10^(11 :: Integer)

prop_verify_valid :: MCFPrefix -> UniquePair Credential -> Property
prop_verify_valid mp (UniquePair good bad) = testIO $ do
  ch <- hash mp good
  r1 <- verify ch good
  r2 <- verify ch bad
  pure $ (r1, r2) === (Verified, NotVerified)

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

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 } )
