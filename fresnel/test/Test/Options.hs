module Test.Options
( Options(..)
, defaultOptions
) where

import Test.QuickCheck (Args(..), stdArgs)

data Options = Options
  { entries :: [String]
  , args    :: Args
  }

defaultOptions :: Options
defaultOptions = Options{ entries = [], args = stdArgs{ maxSuccess = 250, chatty = False }}
