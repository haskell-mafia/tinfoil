import           Disorder.Core.Main

import qualified Test.Tinfoil.Random

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Random.tests
  ]
