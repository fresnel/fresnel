module Main
( main
) where

import qualified Profunctor.Coexp.Test

main :: IO ()
main = sequence_
  [ Profunctor.Coexp.Test.test
  ]
