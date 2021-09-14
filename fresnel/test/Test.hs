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
import           Data.Maybe (catMaybes)
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
main = traverse (runGroup stdArgs{ maxSuccess = 250, chatty = False } initialIndent . uncurry Group . fmap (map (uncurry Case)))
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

newtype Indent = Indent { getIndent :: [IO () -> IO ()] }

initialIndent :: Indent
initialIndent = Indent []

push :: (IO () -> IO ()) -> Indent -> Indent
push f (Indent fs) = Indent (f:fs)

putIndentStrLn :: Indent -> String -> IO ()
putIndentStrLn i = indenting i . putStrLn

indenting :: Indent -> IO () -> IO ()
indenting i = foldr (.) id (getIndent i)

runGroup :: Args -> Indent -> Group -> IO (Int, Int)
runGroup args indent Group{ groupName, cases } = do
  withSGR [setBold, setColour Magenta] $
    putStrLn groupName
  withSGR [setColour Magenta] $ putStrLn ("┌─" ++ replicate (length groupName - 2) '─')
  let indent' = push (withSGR [setColour Magenta] (putStr "│ ") *>) indent
  rs <- catMaybes <$> sequence (intersperse (Nothing <$ putIndentStrLn indent' "") (map (fmap Just <$> runCase args indent') cases))
  tally (length (filter id rs), length (filter not rs))

runCase :: Args -> Indent -> Case -> IO Bool
runCase args indent Case{ caseName, property } = do
  let ident = case breaks [isSpace, not . isSpace, isSpace, not . isSpace] caseName of
        [propName, _, _, _, loc] -> Just (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') propName)), loc)
        _                        -> Nothing
  r <- quickCheckWithResult args property
  result indent ident r
  pure (isSuccess r)

result :: Indent -> Maybe (String, String) -> Result -> IO ()
result indent ident = \case
  Success{ numTests, numDiscarded, labels, classes, tables } -> do
    indenting indent $ do
      header
      success $ putStr "Success."
      putStr " "
      stats $ Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 }
      Main.classes numTests classes
      putStrLn "."
    Main.labels indent numTests labels
    Main.tables indent numTests tables

  GaveUp{ numTests, numDiscarded, labels, classes, tables } -> do
    indenting indent $ do
      header
      failure $ putStr "Failure."
      putStr " "
      stats $ Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 }
      Main.classes numTests classes
      putStrLn ":"
    Main.labels indent numTests labels
    Main.tables indent numTests tables

  Failure{ numTests, numDiscarded, numShrinks, usedSeed, usedSize, reason, theException, failingTestCase, failingLabels, failingClasses } -> do
    indenting indent $ do
      header
      failure $ putStr "Failure."
      putStr " "
      stats $ Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks }
      putStrLn ":"
    for_ ident (putIndentStrLn indent . snd)
    putStrLn ""
    putIndentStrLn indent reason
    for_ theException (putIndentStrLn indent . displayException)
    for_ failingTestCase (putIndentStrLn indent)
    putStrLn ""
    putIndentStrLn indent ("Seed: " ++ show usedSeed)
    putIndentStrLn indent ("Size: " ++ show usedSize)
    unless (null failingLabels)  $ putIndentStrLn indent ("Labels: "  ++ intercalate ", " failingLabels)
    unless (null failingClasses) $ putIndentStrLn indent ("Classes: " ++ intercalate ", " (toList failingClasses))

  NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables } -> do
    indenting indent $ do
      header
      failure $ putStr "Failure."
      putStr " "
      stats $ Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 }
      Main.classes numTests classes
      putStr ":"
    Main.labels indent numTests labels
    Main.tables indent numTests tables
  where
  header = for_ ident $ \ (name, _) -> do
    withSGR [setBold] (putStr name)
    putStr " … "

data Stats = Stats
  { numTests     :: Int
  , numDiscarded :: Int
  , numShrinks   :: Int
  }

stats :: Stats -> IO ()
stats Stats{ numTests, numDiscarded, numShrinks } = do
  sequence_ . intersperse (putStr ", ")
    $  toList (stat (S "test") numTests)
    ++ toList (stat (S "discard") numDiscarded)
    ++ toList (stat (S "shrink") numShrinks)


labels :: Indent -> Int -> Map.Map [String] Int -> IO ()
labels indent n labels = unless (null labels) (putIndentStrLn indent "") *> traverse_ (table n . sortBy (flip (comparing snd) <> flip (comparing fst)) . Map.toList) (IntMap.elems numberedLabels) where
  numberedLabels = IntMap.fromListWith (Map.unionWith (+)) $
    [ (i, Map.singleton l n)
    | (labels, n) <- Map.toList labels,
      (i, l) <- zip [(0 :: Int)..] labels
    ]
  table k m = do
    for_ m $ \ (key, v) -> do
      let percentage = fromIntegral v / fromIntegral k * 100 :: Double
      putIndentStrLn indent $ (if percentage < 10 then " " else "") ++ showFFloatAlt (Just 1) percentage "" ++ "% " ++ key
    putStrLn ""

classes :: Int -> Map.Map String Int -> IO ()
classes n classes = unless (null classes) $ do
  putStr " "
  parens $ sequence_ (intersperse (putStr ", ") (map (uncurry (class_ n)) (Map.toList classes)))

class_ :: Int -> String -> Int -> IO ()
class_ n label n' = let percentage = fromIntegral n' / fromIntegral n * 100 :: Double in putStr (showFFloatAlt (Just 1) percentage ('%':' ':label))

tables :: Indent -> Int -> Map.Map String (Map.Map String Int) -> IO ()
tables _ _ _ = pure ()


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
