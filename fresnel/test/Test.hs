{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main
( main
) where

import qualified Fold.Test
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import qualified Profunctor.Coexp.Test
import qualified Review.Test
import           System.Environment (getArgs)
import           Test.Options
import           Test.Run
import qualified Tropical.Test

main :: IO ()
main = getArgs >>= withOptions defaultOpts (runEntries entries)
  where
  entries =
    [ Fold.Test.tests
    , Getter.Test.tests
    , Iso.Test.tests
    , Monoid.Fork.Test.tests
    , Profunctor.Coexp.Test.tests
    , Review.Test.tests
    , Tropical.Test.tests
    ]
