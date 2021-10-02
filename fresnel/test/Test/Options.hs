{-# LANGUAGE NamedFieldPuns #-}
module Test.Options
( -- * Options
  Options(..)
, defaultOptions
, entries_
, args_
  -- * Args optics
, maxSuccess_
, maxSize_
, maxShrinks_
, replay_
  -- * CLI options
, parseOpts
) where

import Fresnel.Lens (Lens', lens)
import System.Console.GetOpt
import Test.QuickCheck (Args(..), stdArgs)
import Test.QuickCheck.Random (QCGen)

-- Options

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


-- Args optics

maxSuccess_ :: Lens' Args Int
maxSuccess_ = lens maxSuccess (\ a maxSuccess -> a{ maxSuccess })

maxSize_ :: Lens' Args Int
maxSize_ = lens maxSize (\ a maxSize -> a{ maxSize })

maxShrinks_ :: Lens' Args Int
maxShrinks_ = lens maxShrinks (\ a maxShrinks -> a{ maxShrinks })

replay_ :: Lens' Args (Maybe (QCGen, Int))
replay_ = lens replay (\ a replay -> a{ replay })


-- CLI options

parseOpts :: [OptDescr (Options -> Options)] -> [String] -> Either [String] Options
parseOpts opts args
  | null other
  , null errs = Right options
  | otherwise = Left (map ("Unrecognized argument: " ++) other ++ errs)
  where
  options = foldr ($) defaultOptions mods
  (mods, other, errs) = getOpt RequireOrder opts args
