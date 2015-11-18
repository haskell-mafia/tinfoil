import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Random

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Random.tests
  ]
