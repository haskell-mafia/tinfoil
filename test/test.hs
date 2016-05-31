import           Disorder.Core.Main

import qualified Test.Tinfoil.Data.KDF
import qualified Test.Tinfoil.Data.MAC
import qualified Test.Tinfoil.Data.Signing
import qualified Test.Tinfoil.Hash
import qualified Test.Tinfoil.Hash.TestVectors
import qualified Test.Tinfoil.KDF.Scrypt
import qualified Test.Tinfoil.MAC
import qualified Test.Tinfoil.Random
import qualified Test.Tinfoil.Signing.Ed25519.Internal

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Data.KDF.tests
  , Test.Tinfoil.Data.MAC.tests
  , Test.Tinfoil.Data.Signing.tests
  , Test.Tinfoil.Hash.tests
  , Test.Tinfoil.Hash.TestVectors.tests
  , Test.Tinfoil.MAC.tests
  , Test.Tinfoil.KDF.Scrypt.tests
  , Test.Tinfoil.Random.tests
  , Test.Tinfoil.Signing.Ed25519.Internal.tests
  ]
