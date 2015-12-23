import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Random
import qualified Test.IO.Tinfoil.KDF
import qualified Test.IO.Tinfoil.KDF.Scrypt

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Random.tests
  , Test.IO.Tinfoil.KDF.tests
  , Test.IO.Tinfoil.KDF.Scrypt.tests
  ]
