module Main
( main
) where

import qualified Getter.Test
import qualified Profunctor.Coexp.Test

main :: IO ()
main = sequence_
  [ Getter.Test.test
  , Profunctor.Coexp.Test.test
  ]
