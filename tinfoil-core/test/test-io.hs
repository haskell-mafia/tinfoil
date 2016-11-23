import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Core.Comparison
import qualified Test.IO.Tinfoil.Core.Data.MAC
import qualified Test.IO.Tinfoil.Core.Hash
import qualified Test.IO.Tinfoil.Core.KDF
import qualified Test.IO.Tinfoil.Core.KDF.Scrypt
import qualified Test.IO.Tinfoil.Core.KDF.Scrypt.Compat
import qualified Test.IO.Tinfoil.Core.MAC
import qualified Test.IO.Tinfoil.Core.Random

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Core.Comparison.tests
  , Test.IO.Tinfoil.Core.Data.MAC.tests
  , Test.IO.Tinfoil.Core.Hash.tests
  , Test.IO.Tinfoil.Core.KDF.tests
  , Test.IO.Tinfoil.Core.KDF.Scrypt.tests
  , Test.IO.Tinfoil.Core.KDF.Scrypt.Compat.tests
  , Test.IO.Tinfoil.Core.MAC.tests
  , Test.IO.Tinfoil.Core.Random.tests
  ]
