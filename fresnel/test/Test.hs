{-# LANGUAGE LambdaCase #-}
module Main
( main
) where

import           Data.Bool (bool)
import           Data.Char (isSpace)
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Colour.SRGB
import           Data.Traversable (for)
import qualified Fold.Test
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import qualified Profunctor.Coexp.Test
import           System.Console.ANSI
import           System.Exit (exitFailure, exitSuccess)
import           Test.QuickCheck

main :: IO ()
main = traverse (uncurry (runQuickCheckAll (quickCheckWithResult stdArgs{ maxSuccess = 250 })))
  [ Fold.Test.tests
  , Getter.Test.tests
  , Iso.Test.tests
  , Monoid.Fork.Test.tests
  , Profunctor.Coexp.Test.tests
  ]
  >>= bool exitFailure exitSuccess . and

runQuickCheckAll :: (Property -> IO Result) -> String -> [(String, Property)] -> IO Bool
runQuickCheckAll qc __FILE__ ps = do
  withSGR [setBold, setRGB (hsl 300 1 0.75)] $
    putStrLn __FILE__
  putStrLn ""
  rs <- for ps $ \ (xs, p) -> do
    pos <- getCursorPosition
    let header colour = case breaks [isSpace, not . isSpace, isSpace, not . isSpace] xs of
          [propName, _, _, _, loc] -> do
            withSGR [setBold, setRGB colour] $
              putStr (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') propName)))
            putStrLn (' ' : '(' : loc ++ ")")
          _ -> pure ()
    header (hsl 180 1 0.35)
    r <- qc p
    if isSuccess r then
      pure ()
    else do
      saveCursor
      maybe (pure ()) (uncurry setCursorPosition) pos
      header (hsl 0 1 0.5)
      restoreCursor
    putStrLn ""
    pure (isSuccess r)

  let successes = length (filter id rs)
      hasSuccesses = successes /= 0
      failures = length (filter not rs)
      hasFailures = failures /= 0
  if hasFailures then
    failure $ putStr "Failed:"
  else
    success $ putStr "Succeeded:"
  putStr " "
  if hasSuccesses then
    success $ do
      putStr (show successes)
      putStr " successes"
  else
    putStr "0 successes"
  putStr ", "
  if hasFailures then
    failure $ do
      putStr (show failures)
      putStr " failures"
  else
    putStr "0 failures"
  putStrLn ""
  pure hasFailures

setRGB :: RGB Float -> SGR
setRGB = SetRGBColor Foreground . uncurryRGB sRGB

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity


red :: RGB Float
red = hsl 0 1 0.5

green :: RGB Float
green = hsl 120 1 0.5


withSGR :: [SGR] -> IO a -> IO a
withSGR sgr io = setSGR sgr *> io <* setSGR []

colour :: RGB Float -> IO a -> IO a
colour c = withSGR [setRGB c]

success, failure :: IO a -> IO a

success = colour green
failure = colour red


breaks :: [a -> Bool] -> [a] -> [[a]]
breaks ps as = case ps of
  []   -> [as]
  p:ps -> let (h, t) = break p as in h : breaks ps t

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p = go False where
  go b = \case
    [] -> []
    as -> let (h, t) = break (if b then not . p else p) as in h : go (not b) t
