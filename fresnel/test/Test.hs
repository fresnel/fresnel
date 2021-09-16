{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
( main
) where

import           Control.Monad (unless, when)
import           Data.Foldable (for_, toList)
import qualified Data.IntMap as IntMap
import           Data.List (intercalate, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe)
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
import           Test.Group
import           Test.QuickCheck (Args(..), Result(..), isSuccess, quickCheckWithResult, stdArgs, (.&&.), (===))
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  let i = Indent [putStr "  "]
  res <- traverse (runGroup i stdArgs{ maxSuccess = 250, chatty = False } w) groups
  (_, failures) <- uncurry (tally i) (foldr (\ (s, f) (ss, fs) -> (s + ss, f + fs)) (0, 0) res)
  if failures == 0 then
    exitSuccess
  else
    exitFailure
  where
  groups =
    [ Fold.Test.tests
    , Getter.Test.tests
    , Iso.Test.tests
    , Monoid.Fork.Test.tests
    , Profunctor.Coexp.Test.tests
    , tropical
    ]
  w = fromMaybe 0 (getTropical (maxWidths groups))


newtype Indent = Indent { getIndent :: [IO ()] }

incr :: IO () -> Indent -> Indent
incr i = Indent . (i:) . getIndent

putNewline :: String -> IO ()
putNewline s = putStr s *> newline

line :: Indent -> IO () -> IO ()
line is = foldr (\ each into m -> into (each *> m)) id (getIndent is)

lineStr :: Indent -> String -> IO ()
lineStr i s = line i $ putStr s *> newline

newline :: IO ()
newline = putStrLn ""

runGroup :: Indent -> Args -> Int -> Group -> IO (Int, Int)
runGroup i args width Group{ groupName, cases } = do
  withSGR [setBold] $ lineStr i groupName
  lineStr i (replicate (2 + fullWidth width) '━')
  rs <- catMaybes <$> sequence (intersperse (Nothing <$ newline) (map (fmap Just <$> runCase i args width) cases))
  newline
  tally i (length (filter id rs)) (length (filter not rs))

runCase :: Indent -> Args -> Int -> Case -> IO Bool
runCase i args width Case{ name, loc, property } = do
  r <- quickCheckWithResult args property
  result i width name loc r
  pure (isSuccess r)

result :: Indent -> Int -> String -> Loc -> Result -> IO ()
result i width name Loc{ path, lineNumber } res = case res of
  Success{ numTests, numDiscarded, labels, classes, tables } -> do
    header
    body numTests labels classes tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } "."

  GaveUp{ numTests, numDiscarded, labels, classes, tables } -> do
    header
    body numTests labels classes tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } ":"

  Failure{ numTests, numDiscarded, numShrinks, usedSeed, usedSize, reason, theException, failingTestCase, failingLabels, failingClasses } -> do
    header
    i <- pure (incr (failure (putStr "│ ")) i)
    stats i Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks }
    unless (null failingClasses) $ putStr (" (" ++ intercalate ", " (toList failingClasses) ++ ")")
    putNewline ":"
    lineStr i (path ++ ":" ++ show lineNumber)
    lineStr i reason
    for_ theException (lineStr i . displayException)
    for_ failingTestCase (lineStr i)
    lineStr i ""
    lineStr i ("Seed: " ++ show usedSeed)
    lineStr i ("Size: " ++ show usedSize)
    unless (null failingLabels) . lineStr i $ "Labels: "  ++ intercalate ", " failingLabels

  NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables } -> do
    header
    body numTests labels classes tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } ":"
  where
  header = do
    line i $ do
      let δ = width - length name
      withSGR [setBold] (putStr "❧ " *> putStr name *> when (width > 0) (putStr (replicate δ ' ')))
      putStr "   "
      status . putNewline $ if succeeded then "Success." else "Failure."

    let gutter
          | succeeded = putStr "  "
          | otherwise = status (putStr "╭─")
    i <- pure (incr gutter i)
    line i . status $ putNewline (replicate (fullWidth width) '─')

  body numTests labels classes tables s t = do
    i <- pure (incr (putStr "  ") i)
    sequence_ (intersperse (lineStr i "") ((stats i s *> Main.classes i numTests classes *> putNewline t) : Main.labels i numTests labels))
    Main.tables i numTests tables

  succeeded = case res of
    Success{} -> True
    _         -> False

  status
    | succeeded = success
    | otherwise = failure

data Stats = Stats
  { numTests     :: Int
  , numDiscarded :: Int
  , numShrinks   :: Int
  }

stats :: Indent -> Stats -> IO ()
stats i Stats{ numTests, numDiscarded, numShrinks } = line i . sequence_ . intersperse (putStr ", ")
  $  toList (stat (S "test") numTests)
  ++ toList (stat (S "discard") numDiscarded)
  ++ toList (stat (S "shrink") numShrinks)


labels :: Indent -> Int -> Map.Map [String] Int -> [IO ()]
labels i n labels
  | null labels = []
  | otherwise   = map (table n . sortBy (flip (comparing snd) <> flip (comparing fst)) . Map.toList) (IntMap.elems numberedLabels)
  where
  numberedLabels = IntMap.fromListWith (Map.unionWith (+)) $
    [ (i, Map.singleton l n)
    | (labels, n) <- Map.toList labels
    , (i, l) <- zip [(0 :: Int)..] labels
    ]
  table k m = for_ m $ \ (key, v) -> do
    let percentage = fromIntegral v / fromIntegral k * 100 :: Double
    lineStr i $ (if percentage < 10 then " " else "") ++ showFFloatAlt (Just 1) percentage "" ++ "% " ++ key

classes :: Indent -> Int -> Map.Map String Int -> IO ()
classes i n classes = unless (null classes) $ do
  putStr " "
  parens $ sequence_ (intersperse (putStr ", ") (map (uncurry (class_ i n)) (Map.toList classes)))

class_ :: Indent -> Int -> String -> Int -> IO ()
class_ _ n label n' = let percentage = fromIntegral n' / fromIntegral n * 100 :: Double in putStr (showFFloatAlt (Just 1) percentage ('%':' ':label))

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

fullWidth :: Int -> Int
fullWidth width = width + 3 + length "Success."

stat :: Plural -> Int -> Maybe (IO ())
stat _    0 = Nothing
stat name n = Just $ do
  putStr (show n)
  putStr " "
  putStr (pluralize n name)

tally :: Indent -> Int -> Int -> IO (Int, Int)
tally i successes failures = ((successes, failures) <$) . line i $ do
  let hasSuccesses = successes /= 0
      hasFailures = failures /= 0
  when hasSuccesses . success $ do
    putStr (show successes)
    putStr (' ' : pluralize successes (C "success" "successes"))
  when (hasSuccesses && hasFailures) $ putStr ", "
  when hasFailures . failure $ do
    putStr (show failures)
    putStr (' ' : pluralize failures (S "failure"))
  when (hasSuccesses || hasFailures) (putNewline ".")
  newline

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

tropical :: Group
tropical = Group
  { groupName = "Test.Group.Tropical"
  , cases =
    [ semigroupAssoc
    , monoidIdentity
    ]
  }
  where
  semigroupAssoc = Case{ name = "semigroup assoc", loc = here, property = QC.property (\ (ArbTropical a) (ArbTropical b) (ArbTropical c) -> a <> (b <> c) === (a <> b) <> c) }
  monoidIdentity = Case{ name = "monoid identity", loc = here, property = QC.property (\ (ArbTropical a) -> (mempty <> a) === a .&&. (a <> mempty) === a)}

newtype ArbTropical = ArbTropical (Tropical Int)
  deriving (Eq, Ord, Show)

instance QC.Arbitrary ArbTropical where
  arbitrary = QC.oneof
    [ pure (ArbTropical (Tropical Nothing))
    , ArbTropical . Tropical . Just <$> QC.arbitrary
    ]
