import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Random
import qualified Test.IO.Tinfoil.KDF

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Random.tests
  , Test.IO.Tinfoil.KDF.tests
  ]
