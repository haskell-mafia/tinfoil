{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Tinfoil.Internal.Sodium where

import           Disorder.Core.IO (testIO)

import           P

import           System.IO

import           Test.QuickCheck

import           Tinfoil.Internal.Sodium

import           X.Control.Monad.Trans.Either (runEitherT)

prop_initialiseSodium :: Property
prop_initialiseSodium = once . testIO $ do
  r1 <- runEitherT initialiseSodium
  r2 <- runEitherT initialiseSodium
  pure $ (r1, isRight r1, isRight r2) === (r2, True, True)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
