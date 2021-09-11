module Main
( main
) where

import qualified Fold.Test
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import qualified Profunctor.Coexp.Test

main :: IO ()
main = sequence_
  [ Fold.Test.test
  , Getter.Test.test
  , Iso.Test.test
  , Monoid.Fork.Test.test
  , Profunctor.Coexp.Test.test
  ]
