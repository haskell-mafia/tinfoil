import           Disorder.Core.Main

import qualified Test.Tinfoil.Data.KDF
import qualified Test.Tinfoil.Data.Signing
import qualified Test.Tinfoil.Hash
import qualified Test.Tinfoil.Hash.TestVectors
import qualified Test.Tinfoil.Random
import qualified Test.Tinfoil.KDF.Scrypt

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Data.KDF.tests
  , Test.Tinfoil.Data.Signing.tests
  , Test.Tinfoil.Hash.tests
  , Test.Tinfoil.Hash.TestVectors.tests
  , Test.Tinfoil.Random.tests
  , Test.Tinfoil.KDF.Scrypt.tests
  ]
