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
import           Fresnel.Lens (Lens', lens)
import           Fresnel.Setter
import           GHC.Exception.Type (Exception(displayException))
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import           Numeric (readDec, showFFloatAlt)
import qualified Profunctor.Coexp.Test
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO
import           Test.Group
import           Test.QuickCheck (Args(..), Result(..), isSuccess, quickCheckWithResult, stdArgs, (.&&.), (===))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)

main :: IO ()
main = do
  let opts =
        [ Option "n" ["successes"] (ReqArg (set (args_.maxSuccess_)        . int) "N") "require N successful tests before concluding the property passes"
        , Option "z" ["size"]      (ReqArg (set (args_.maxSize_)           . int) "N") "increase the size parameter to a maximum of N for successive tests of a property"
        , Option "s" ["shrinks"]   (ReqArg (set (args_.maxShrinks_)        . int) "N") "perform a maximum of N shrinks; setting this to 0 disables shrinking"
        , Option "g" ["group"]     (ReqArg (\ s -> groups_ %~ (s:))            "NAME") "include the named group; can be used multiple times to include multiple groups"
        , Option "r" ["replay"]    (ReqArg (set (args_.replay_) . Just . read) "SEED") "the seed and size to repeat"
        ]
      i = Indent [putStr "  "]
  (mods, other, errs) <- getOpt RequireOrder opts <$> getArgs
  case map ("Unrecognized argument: " ++) other ++ errs of
    [] -> pure ()
    _  -> do
      name <- getProgName
      for_ (errs ++ [usageInfo (header name) opts]) (hPutStrLn stderr)
  let Options gs _ args = foldr ($) (Options{ groups = [], cases = [], args = stdArgs{ maxSuccess = 250, chatty = False }}) mods
      matching _ [] = id
      matching f fs = filter (\ g -> foldr ((||) . f g) False fs)
  res <- traverse (runGroup i args w) (matching ((==) . groupName) gs groups)
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
  int = fst . head . readDec
  header name = "Usage: " ++ name ++ " [-n N|--successes N]"

maxSuccess_ :: Lens' Args Int
maxSuccess_ = lens maxSuccess (\ a maxSuccess -> a{ maxSuccess })

maxSize_ :: Lens' Args Int
maxSize_ = lens maxSize (\ a maxSize -> a{ maxSize })

maxShrinks_ :: Lens' Args Int
maxShrinks_ = lens maxShrinks (\ a maxShrinks -> a{ maxShrinks })

replay_ :: Lens' Args (Maybe (QCGen, Int))
replay_ = lens replay (\ a replay -> a{ replay })

data Options = Options
  { groups :: [String]
  , cases  :: [String]
  , args   :: Args
  }

groups_ :: Lens' Options [String]
groups_ = lens groups (\ o groups -> o{ groups })

args_ :: Lens' Options Args
args_ = lens args (\ o args -> o{ args })


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
runCase i args width Case{ name, loc = Loc{ path, lineNumber }, property } = do
  res <- quickCheckWithResult args property
  let succeeded f t = if isSuccess res then t else f
      status = if isSuccess res then success else failure
      gutter = succeeded (status (putStr "╭─")) (putStr "  ")

      header = do
        line i $ do
          let δ = width - length name
          withSGR [setBold] (putStr "❧ " *> putStr name *> when (width > 0) (putStr (replicate δ ' ')))
          putStr "   "
          status . putNewline $ succeeded "Failure." "Success."

        i <- pure (incr gutter i)
        line i . status $ putNewline (replicate (fullWidth width) '─')

  case res of
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
      lineStr i ("--replay (" ++ show usedSeed ++ "," ++ show usedSize ++ ")")
      unless (null failingLabels) . lineStr i $ "Labels: "  ++ intercalate ", " failingLabels

    NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables } -> do
      header
      body numTests labels classes tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } ":"
  pure (isSuccess res)
  where
  body numTests labels classes tables s t = do
    i <- pure (incr (putStr "  ") i)
    sequence_ (intersperse (lineStr i "") ((stats i s *> Main.classes i numTests classes *> putNewline t) : Main.labels i numTests labels))
    Main.tables i numTests tables

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
