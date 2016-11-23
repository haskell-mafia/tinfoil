import           Disorder.Core.Main

import qualified Test.Tinfoil.Core.Data.Hash
import qualified Test.Tinfoil.Core.Data.KDF
import qualified Test.Tinfoil.Core.Data.Key
import qualified Test.Tinfoil.Core.Data.MAC
import qualified Test.Tinfoil.Core.Encode
import qualified Test.Tinfoil.Core.Hash
import qualified Test.Tinfoil.Core.Hash.TestVectors
import qualified Test.Tinfoil.Core.KDF.Scrypt
import qualified Test.Tinfoil.Core.MAC
import qualified Test.Tinfoil.Core.Random

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Core.Data.Hash.tests
  , Test.Tinfoil.Core.Data.KDF.tests
  , Test.Tinfoil.Core.Data.Key.tests
  , Test.Tinfoil.Core.Data.MAC.tests
  , Test.Tinfoil.Core.Encode.tests
  , Test.Tinfoil.Core.Hash.tests
  , Test.Tinfoil.Core.Hash.TestVectors.tests
  , Test.Tinfoil.Core.MAC.tests
  , Test.Tinfoil.Core.KDF.Scrypt.tests
  , Test.Tinfoil.Core.Random.tests
  ]
