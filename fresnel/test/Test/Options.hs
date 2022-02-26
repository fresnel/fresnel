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
, defaultOpts
, printErrors
, header
, withOptions
) where

import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Fresnel.Lens (Lens', lens)
import Fresnel.Setter (set, (%~))
import Numeric (readDec)
import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
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

defaultOpts :: [OptDescr (Options -> Options)]
defaultOpts =
  [ Option "n" ["successes"] (ReqArg (set (args_.maxSuccess_)    . int)  "N")    "require N successful tests before concluding the property passes"
  , Option "z" ["size"]      (ReqArg (set (args_.maxSize_)       . int)  "N")    "increase the size parameter to a maximum of N for successive tests of a property"
  , Option "s" ["shrinks"]   (ReqArg (set (args_.maxShrinks_)    . int)  "N")    "perform a maximum of N shrinks; setting this to 0 disables shrinking"
  , Option "m" ["match"]     (ReqArg (\ s -> entries_ %~ (s:))           "NAME") "include the named group or property; can be used multiple times to include multiple groups/properties"
  , Option "r" ["replay"]    (ReqArg (set (args_.replay_) . Just . read) "SEED") "the seed and size to repeat"
  ]
  where
  int = fst . head . readDec

printErrors :: [OptDescr a] -> [String] -> IO Bool
printErrors opts errs = do
  name <- getProgName
  False <$ traverse_ (hPutStrLn stderr) (errs ++ [usageInfo (header name opts) opts])

header :: String -> [OptDescr a] -> String
header name opts = "Usage: " ++ name ++ " " ++ unwords (map opt opts) where
  opt (Option short long a _) = bracket (intercalate "|" ([ arg a ['-',c] | c <- short ] ++ map (arg a . ("--" ++)) long))
  arg (NoArg _)    = id
  arg (ReqArg _ s) = (++ " " ++ s)
  arg (OptArg _ s) = (++ bracket s)
  bracket s = "[" ++ s ++ "]"

withOptions :: [OptDescr (Options -> Options)] -> (Options -> IO Bool) -> [String] -> IO ()
withOptions opts f = bool exitFailure exitSuccess <=< either (printErrors opts) f . parseOpts opts
