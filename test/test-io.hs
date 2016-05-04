import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Random
import qualified Test.IO.Tinfoil.KDF.Common
import qualified Test.IO.Tinfoil.KDF.Scrypt
import qualified Test.IO.Tinfoil.KDF.Scrypt.Compat

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Random.tests
  , Test.IO.Tinfoil.KDF.Common.tests
  , Test.IO.Tinfoil.KDF.Scrypt.tests
  , Test.IO.Tinfoil.KDF.Scrypt.Compat.tests
  ]
