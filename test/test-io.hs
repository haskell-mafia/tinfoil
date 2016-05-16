import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Hash
import qualified Test.IO.Tinfoil.KDF
import qualified Test.IO.Tinfoil.KDF.Common
import qualified Test.IO.Tinfoil.KDF.Scrypt
import qualified Test.IO.Tinfoil.KDF.Scrypt.Compat
import qualified Test.IO.Tinfoil.Random

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Hash.tests
  , Test.IO.Tinfoil.KDF.tests
  , Test.IO.Tinfoil.KDF.Common.tests
  , Test.IO.Tinfoil.KDF.Scrypt.tests
  , Test.IO.Tinfoil.KDF.Scrypt.Compat.tests
  , Test.IO.Tinfoil.Random.tests
  ]
