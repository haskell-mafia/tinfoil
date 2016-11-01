{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tinfoil.Data.Key where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO

import           Tinfoil.Data.Key

import           Test.QuickCheck
import           Test.Tinfoil.Arbitrary ()

prop_tripping_SymmetricKey :: SymmetricKey -> Property
prop_tripping_SymmetricKey = tripping renderSymmetricKey parseSymmetricKey

prop_show_SymmetricKey :: SymmetricKey -> Property
prop_show_SymmetricKey sk@(SymmetricKey bs) =
  counterexample (show sk) $ not . BS.isInfixOf bs . BSC.pack $ show sk

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
