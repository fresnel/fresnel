{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
( main
) where

import           Control.Monad (unless)
import           Data.Bool (bool)
import           Data.Char (isSpace)
import           Data.Foldable (for_, toList, traverse_)
import qualified Data.IntMap as IntMap
import           Data.List (intercalate, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Ord (comparing)
import qualified Fold.Test
import           GHC.Exception.Type (Exception(displayException))
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import           Numeric (showFFloatAlt)
import qualified Profunctor.Coexp.Test
import           System.Console.ANSI
import           System.Exit (exitFailure, exitSuccess)
import           Test.QuickCheck

main :: IO ()
main = traverse (runGroup stdArgs{ maxSuccess = 250, chatty = False } . uncurry Group . fmap (map (uncurry Case)))
  [ Fold.Test.tests
  , Getter.Test.tests
  , Iso.Test.tests
  , Monoid.Fork.Test.tests
  , Profunctor.Coexp.Test.tests
  ]
  >>= tally . foldr (\ (s, f) (ss, fs) -> (s + ss, f + fs)) (0, 0)
  >>= bool exitFailure exitSuccess . (== 0) . snd

data Group = Group
  { groupName :: String
  , cases     :: [Case]
  }

data Case = Case
  { caseName :: String
  , property :: Property
  }

runGroup :: Args -> Group -> IO (Int, Int)
runGroup args Group{ groupName, cases } = do
  withSGR [setBold, setColour Magenta] $
    putStrLn groupName
  putStrLn ""
  rs <- traverse (runCase args) cases

  tally (length (filter id rs), length (filter not rs))

runCase :: Args -> Case -> IO Bool
runCase args Case{ caseName, property } = do
  loc <- case breaks [isSpace, not . isSpace, isSpace, not . isSpace] caseName of
    [propName, _, _, _, loc] -> do
      withSGR [setBold] $
        putStrLn (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') propName)))
      pure (Just loc)
    _ -> pure Nothing
  r <- quickCheckWithResult args property
  result loc r
  putStrLn ""
  pure (isSuccess r)

result :: Maybe String -> Result -> IO ()
result loc = \case
  Success{ numTests, numDiscarded, labels, classes, tables } -> do
    success $ putStr "OK "
    parens $ stats $ emptyStats{ Main.numTests, Main.numDiscarded }
    putStrLn ""
    Main.labels numTests labels
    Main.classes numTests classes
    Main.tables numTests tables
  GaveUp{ numTests, numDiscarded, labels, classes, tables } -> do
    failure $ putStr "FAIL "
    parens $ stats $ emptyStats{ Main.numTests, Main.numDiscarded }
    putStrLn ""
    Main.labels numTests labels
    Main.classes numTests classes
    Main.tables numTests tables
  Failure{ numTests, numDiscarded, numShrinks, usedSeed, usedSize, reason, theException, failingTestCase, failingLabels, failingClasses } -> do
    maybe (pure ()) putStrLn loc
    failure $ putStr "FAIL "
    parens $ stats $ emptyStats{ Main.numTests, Main.numDiscarded, Main.numShrinks }
    putStrLn ":"
    putStrLn ""
    putStrLn reason
    maybe (pure ()) (putStrLn . displayException) theException
    traverse_ putStrLn failingTestCase
    putStrLn ""
    putStrLn ("Seed: " ++ show usedSeed)
    putStrLn ("Size: " ++ show usedSize)
    unless (null failingLabels) $ putStrLn ("Labels: " ++ intercalate ", " failingLabels)
    unless (null failingClasses) $ putStrLn ("Classes: " ++ intercalate ", " (toList failingClasses))
  NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables } -> do
    failure $ putStr "FAIL "
    parens $ stats $ emptyStats{ Main.numTests, Main.numDiscarded }
    putStrLn ""
    Main.labels numTests labels
    Main.classes numTests classes
    Main.tables numTests tables

data Stats = Stats
  { numTests     :: Int
  , numDiscarded :: Int
  , numShrinks   :: Int
  }

emptyStats :: Stats
emptyStats = Stats
  { numTests       = 0
  , numDiscarded   = 0
  , numShrinks     = 0
  }

stats :: Stats -> IO ()
stats Stats{ numTests, numDiscarded, numShrinks } = do
  sequence_ . intersperse (putStr ", ")
    $  toList (stat (S "test") numTests)
    ++ toList (stat (S "discard") numDiscarded)
    ++ toList (stat (S "shrink") numShrinks)


labels :: Int -> Map.Map [String] Int -> IO ()
labels n labels = traverse_ (table n . sortBy (flip (comparing snd) <> flip (comparing fst)) . Map.toList) (IntMap.elems numberedLabels) where
  numberedLabels = IntMap.fromListWith (Map.unionWith (+)) $
    [ (i, Map.singleton l n)
    | (labels, n) <- Map.toList labels,
      (i, l) <- zip [(0 :: Int)..] labels
    ]
  table k m = do
    for_ m $ \ (key, v) -> do
      let percentage = fromIntegral v / fromIntegral k * 100 :: Double
      putStrLn $ (if percentage < 10 then " " else "") ++ showFFloatAlt (Just 1) percentage "" ++ "% " ++ key
    putStrLn ""

classes :: Int -> Map.Map String Int -> IO ()
classes n classes = traverse_ (\ (label, n') -> let percentage = fromIntegral n' / fromIntegral n * 100 :: Double in putStrLn (showFFloatAlt (Just 1) percentage ('%':' ':label))) (Map.toList classes)

tables :: Int -> Map.Map String (Map.Map String Int) -> IO ()
tables _ _ = pure ()


data Plural
  = S String
  | C String String

pluralize :: Int -> Plural -> String
pluralize 1 = \case
  S s   -> s
  C s _ -> s
pluralize _ = \case
  S   s -> s ++ "s"
  C _ s -> s

stat :: Plural -> Int -> Maybe (IO ())
stat _    0 = Nothing
stat name n = Just $ do
  putStr (show n)
  putStr " "
  putStr (pluralize n name)

tally :: (Int, Int) -> IO (Int, Int)
tally (successes, failures) = do
  let hasSuccesses = successes /= 0
      hasFailures = failures /= 0
  if hasFailures then
    failure $ putStr "Failed:"
  else
    success $ putStr "Succeeded:"
  putStr " "
  if hasSuccesses then
    success $ do
      putStr (show successes)
      putStr (if successes == 1 then " success"  else" successes")
  else
    putStr "0 successes"
  putStr ", "
  if hasFailures then
    failure $ do
      putStr (show failures)
      putStr (if failures == 1 then " failure" else " failures")
  else
    putStr "0 failures"
  putStrLn ""
  putStrLn ""
  pure (successes, failures)

setColour :: Color -> SGR
setColour = SetColor Foreground Vivid

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity


parens :: IO a -> IO a
parens m = do
  putStr "("
  a <- m
  a <$ putStr ")"


withSGR :: [SGR] -> IO a -> IO a
withSGR sgr io = setSGR sgr *> io <* setSGR []

colour :: Color -> IO a -> IO a
colour c = withSGR [setColour c]

success, failure :: IO a -> IO a

success = colour Green
failure = colour Red


breaks :: [a -> Bool] -> [a] -> [[a]]
breaks ps as = case ps of
  []   -> [as]
  p:ps -> let (h, t) = break p as in h : breaks ps t

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p = go False where
  go b = \case
    [] -> []
    as -> let (h, t) = break (if b then not . p else p) as in h : go (not b) t
