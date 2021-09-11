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
main = traverse (uncurry (runQuickCheckAll quickCheckResult))
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
  rs <- for ps $ \ (xs, p) -> do
    case breaks [isSpace, not . isSpace, isSpace, not . isSpace] xs of
      [propName, _, _, _, loc] -> do
        withSGR [setBold, setRGB (hsl 180 1 0.35)] $
          putStr (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') propName)))
        putStrLn (' ' : '(' : loc ++ ")")
      _ -> pure ()
    r <- qc p
    putStrLn ""
    pure (isSuccess r)
  pure (and rs)

setRGB :: RGB Float -> SGR
setRGB = SetRGBColor Foreground . uncurryRGB sRGB

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity


withSGR :: [SGR] -> IO a -> IO a
withSGR sgr io = setSGR sgr *> io <* setSGR []


breaks :: [a -> Bool] -> [a] -> [[a]]
breaks ps as = case ps of
  []   -> [as]
  p:ps -> let (h, t) = break p as in h : breaks ps t

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p = go False where
  go b = \case
    [] -> []
    as -> let (h, t) = break (if b then not . p else p) as in h : go (not b) t
