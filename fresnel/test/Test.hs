module Main
( main
) where

import           Data.Bool (bool)
import qualified Fold.Test
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import qualified Profunctor.Coexp.Test
import           System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = sequence
  [ Fold.Test.test
  , Getter.Test.test
  , Iso.Test.test
  , Monoid.Fork.Test.test
  , Profunctor.Coexp.Test.test
  ]
  >>= bool exitFailure exitSuccess . and
