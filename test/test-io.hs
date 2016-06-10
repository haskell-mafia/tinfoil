import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Comparison
import qualified Test.IO.Tinfoil.Data.MAC
import qualified Test.IO.Tinfoil.Hash
import qualified Test.IO.Tinfoil.KDF
import qualified Test.IO.Tinfoil.KDF.Scrypt
import qualified Test.IO.Tinfoil.KDF.Scrypt.Compat
import qualified Test.IO.Tinfoil.MAC
import qualified Test.IO.Tinfoil.Random
import qualified Test.IO.Tinfoil.Signing.Ed25519
import qualified Test.IO.Tinfoil.Signing.Ed25519.Internal

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Comparison.tests
  , Test.IO.Tinfoil.Data.MAC.tests
  , Test.IO.Tinfoil.Hash.tests
  , Test.IO.Tinfoil.KDF.tests
  , Test.IO.Tinfoil.KDF.Scrypt.tests
  , Test.IO.Tinfoil.KDF.Scrypt.Compat.tests
  , Test.IO.Tinfoil.MAC.tests
  , Test.IO.Tinfoil.Random.tests
  , Test.IO.Tinfoil.Signing.Ed25519.tests
  , Test.IO.Tinfoil.Signing.Ed25519.Internal.tests
  ]
