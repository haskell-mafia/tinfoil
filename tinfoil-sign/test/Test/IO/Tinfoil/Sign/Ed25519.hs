{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Tinfoil.Sign.Ed25519 where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Core.Data
import           Tinfoil.Sign.Ed25519

prop_signMessage :: UniquePair ByteString -> Property
prop_signMessage (UniquePair msg1 msg2) =
  let msg3 = msg1 <> BS.singleton 0x00
      msg4 = BS.singleton 0x00 <> msg1 in testIO $ do
  (pk1, sk1) <- genKeyPair
  (pk2, _sk2) <- genKeyPair
  case signMessage sk1 msg1 of
    Nothing' ->
      pure . failWith $ "Unexpected failure signing: " <> T.pack (show msg1)
    Just' sig ->
      let good = verifyMessage pk1 sig msg1
          bads = [ verifyMessage pk2 sig msg1
                 , verifyMessage pk1 sig msg2
                 , verifyMessage pk1 sig msg3
                 , verifyMessage pk1 sig msg4
                 ] in
      pure $ (good, all (== NotVerified) bads) === (Verified, True)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
