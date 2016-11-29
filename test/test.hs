import           Disorder.Core.Main

import qualified Test.Tinfoil.Data.Hash
import qualified Test.Tinfoil.Data.KDF
import qualified Test.Tinfoil.Data.Key
import qualified Test.Tinfoil.Data.MAC
import qualified Test.Tinfoil.Data.Signing
import qualified Test.Tinfoil.Encode
import qualified Test.Tinfoil.Hash
import qualified Test.Tinfoil.Hash.TestVectors
import qualified Test.Tinfoil.KDF.Scrypt
import qualified Test.Tinfoil.MAC
import qualified Test.Tinfoil.Random

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Data.Hash.tests
  , Test.Tinfoil.Data.KDF.tests
  , Test.Tinfoil.Data.Key.tests
  , Test.Tinfoil.Data.MAC.tests
  , Test.Tinfoil.Data.Signing.tests
  , Test.Tinfoil.Encode.tests
  , Test.Tinfoil.Hash.tests
  , Test.Tinfoil.Hash.TestVectors.tests
  , Test.Tinfoil.MAC.tests
  , Test.Tinfoil.KDF.Scrypt.tests
  , Test.Tinfoil.Random.tests
  ]
