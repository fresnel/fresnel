module Main
( main
) where

import qualified Fresnel.Profunctor.Coexp.Test

main :: IO ()
main = sequence_
  [ Fresnel.Profunctor.Coexp.Test.test
  ]
