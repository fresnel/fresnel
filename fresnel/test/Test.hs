module Main
( main
) where

import qualified Getter.Test
import qualified Iso.Test
import qualified Profunctor.Coexp.Test

main :: IO ()
main = sequence_
  [ Getter.Test.test
  , Iso.Test.test
  , Profunctor.Coexp.Test.test
  ]
