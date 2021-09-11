module Main
( main
) where

import           Data.Bool (bool)
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
runQuickCheckAll qc __FILE__ ps = and <$ setSGR [setBold, setRGB (hsl 300 1 0.75)] <* putStrLn __FILE__ <* setSGR [] <*> for ps (\ (xs, p) -> isSuccess <$ putStrLn ("=== " ++ xs ++ " ===") <*> qc p <* putStrLn "")

setRGB :: RGB Float -> SGR
setRGB = SetRGBColor Foreground . uncurryRGB sRGB

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity
