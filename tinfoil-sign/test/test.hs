import           Disorder.Core.Main

import qualified Test.Tinfoil.Sign.Data
import qualified Test.Tinfoil.Sign.Ed25519.Internal

main :: IO ()
main =
  disorderMain [
    Test.Tinfoil.Sign.Data.tests
  , Test.Tinfoil.Sign.Ed25519.Internal.tests
  ]
