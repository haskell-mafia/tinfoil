import           Disorder.Core.Main

import qualified Test.Tinfoil.Data.Signing
import qualified Test.Tinfoil.Random
import qualified Test.Tinfoil.KDF.Scrypt

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Data.Signing.tests
  , Test.Tinfoil.Random.tests
  , Test.Tinfoil.KDF.Scrypt.tests
  ]
