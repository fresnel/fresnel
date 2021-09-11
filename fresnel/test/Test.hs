module Main
( main
) where

import           Data.Bool (bool)
import           Data.Traversable (for)
import qualified Fold.Test
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import qualified Profunctor.Coexp.Test
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
runQuickCheckAll qc __FILE__ ps = and <$ putStrLn __FILE__ <*> for ps (\ (xs, p) -> isSuccess <$ putStrLn ("=== " ++ xs ++ " ===") <*> qc p <* putStrLn "")
