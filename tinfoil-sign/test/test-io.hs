import           Disorder.Core.Main

import qualified Test.IO.Tinfoil.Sign.Ed25519
import qualified Test.IO.Tinfoil.Sign.Ed25519.Internal

main :: IO ()
main =
  disorderMain [
    Test.IO.Tinfoil.Sign.Ed25519.tests
  , Test.IO.Tinfoil.Sign.Ed25519.Internal.tests
  ]
