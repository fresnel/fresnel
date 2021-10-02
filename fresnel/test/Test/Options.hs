{-# LANGUAGE NamedFieldPuns #-}
module Test.Options
( Options(..)
, defaultOptions
, entries_
, args_
) where

import Fresnel.Lens (Lens', lens)
import Test.QuickCheck (Args(..), stdArgs)

data Options = Options
  { entries :: [String]
  , args    :: Args
  }

defaultOptions :: Options
defaultOptions = Options{ entries = [], args = stdArgs{ maxSuccess = 250, chatty = False }}

entries_ :: Lens' Options [String]
entries_ = lens entries (\ o entries -> o{ entries })

args_ :: Lens' Options Args
args_ = lens args (\ o args -> o{ args })
