{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
( main
, breaks
) where

import           Control.Monad (unless, when)
import           Data.Char (isSpace)
import           Data.Foldable (for_, toList)
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
import           Numeric (readDec, showFFloatAlt)
import qualified Profunctor.Coexp.Test
import           System.Console.ANSI
import           System.Exit (exitFailure, exitSuccess)
import           Test.QuickCheck (Args(..), Property, Result(..), isSuccess, quickCheckWithResult, stdArgs)

main :: IO ()
main = (`runIndentT` initialIndent) $ do
  let groups = map mkGroup
        [ Fold.Test.tests
        , Getter.Test.tests
        , Iso.Test.tests
        , Monoid.Fork.Test.tests
        , Profunctor.Coexp.Test.tests
        ]
      width = maximum [ length (name c) `max` length (groupName g) | g <- groups, c <- cases g ]
  res <- traverse (runGroup stdArgs{ maxSuccess = 250, chatty = False } initialIndent width) groups
  (_, failures) <- tally (foldr (\ (s, f) (ss, fs) -> (s + ss, f + fs)) (0, 0) res)
  lift $ if failures == 0 then
    exitSuccess
  else
    exitFailure

data Group = Group
  { groupName :: String
  , cases     :: [Case]
  }

mkGroup :: (String, [(String, Property)]) -> Group
mkGroup = uncurry Group . fmap (map (uncurry mkCase))

data Case = Case
  { name     :: String
  , loc      :: Loc
  , property :: Property
  }

data Loc = Loc { path :: FilePath, line :: Int }

mkCase :: String -> Property -> Case
mkCase s property = Case{ name, loc = Loc{ path, line }, property }
  where
  (name, path, line) = case breaks [isSpace, not . isSpace, isSpace, not . isSpace, (== ':'), (/= ':')] s of
    [n, _, _, _, p, _, l] -> (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') n)), p, fst (head (readDec l)))
    _                     -> ("", "", 0)

newtype Indent = Indent { getIndent :: [String] }

initialIndent :: Indent
initialIndent = Indent []

push :: String -> Indent -> Indent
push f (Indent fs) = Indent (f:fs)

putIndentStrLn :: Indent -> String -> IndentT IO ()
putIndentStrLn i = indenting i . lift . putStrLn

indenting :: Indent -> IndentT IO () -> IndentT IO ()
indenting i = (lift (putStr (concat (reverse (getIndent i)))) *>)

newline :: IndentT IO ()
newline = lift (putStrLn "")

runGroup :: Args -> Indent -> Int -> Group -> IndentT IO (Int, Int)
runGroup args indent width Group{ groupName, cases } = do
  withSGR [setBold] . lift $
    putStrLn groupName
  lift (putStrLn (replicate (2 + fullWidth width) '━'))
  let indent' = push "  " indent
  rs <- catMaybes <$> sequence (intersperse (Nothing <$ putIndentStrLn indent' "") (map (fmap Just <$> runCase args indent' width) cases))
  newline
  tally (length (filter id rs), length (filter not rs))

runCase :: Args -> Indent -> Int -> Case -> IndentT IO Bool
runCase args indent width Case{ name, loc, property } = do
  r <- lift (quickCheckWithResult args property)
  result indent width name loc r
  pure (isSuccess r)

result :: Indent -> Int -> String -> Loc -> Result -> IndentT IO ()
result indent width name Loc{ path } = \case
  Success{ numTests, numDiscarded, labels, classes, tables } -> do
    header True
    Main.classes numTests classes
    body numTests labels tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } "."

  GaveUp{ numTests, numDiscarded, labels, classes, tables } -> do
    header False
    Main.classes numTests classes
    body numTests labels tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } ":"

  Failure{ numTests, numDiscarded, numShrinks, usedSeed, usedSize, reason, theException, failingTestCase, failingLabels, failingClasses } -> do
    header False
    stats indent Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks }
    unless (null failingClasses) $ lift (putStr (" (" ++ intercalate ", " (toList failingClasses) ++ ")"))
    lift (putStrLn ":")
    putIndentStrLn indent path
    putIndentStrLn indent reason
    for_ theException (putIndentStrLn indent . displayException)
    for_ failingTestCase (putIndentStrLn indent)
    putIndentStrLn indent ""
    putIndentStrLn indent ("Seed: " ++ show usedSeed)
    putIndentStrLn indent ("Size: " ++ show usedSize)
    unless (null failingLabels) $ putIndentStrLn indent ("Labels: "  ++ intercalate ", " failingLabels)

  NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables } -> do
    header False
    Main.classes numTests classes
    body numTests labels tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } ":"
  where
  header succeeded = do
    lift (putStr "❧ ")
    let δ = width - length name
    withSGR [setBold] (lift (putStr name) *> when (width > 0) (lift (putStr (replicate δ ' '))))
    lift (putStr "   ")
    if succeeded then
      success . lift $ putStrLn "Success."
    else
      failure . lift $ putStrLn "Failure."

    indenting indent $ withSGR [setColour (if succeeded then Green else Red)] $ lift (putStrLn (replicate (fullWidth width) '─'))

  body numTests labels tables s t = do
    sequence_ (intersperse (putIndentStrLn indent "") ((stats indent s *> lift (putStrLn t)) : Main.labels indent numTests labels))
    Main.tables indent numTests tables

data Stats = Stats
  { numTests     :: Int
  , numDiscarded :: Int
  , numShrinks   :: Int
  }

stats :: Indent -> Stats -> IndentT IO ()
stats indent Stats{ numTests, numDiscarded, numShrinks } = indenting indent . sequence_ . intersperse (lift (putStr ", "))
  $  toList (stat (S "test") numTests)
  ++ toList (stat (S "discard") numDiscarded)
  ++ toList (stat (S "shrink") numShrinks)


labels :: Indent -> Int -> Map.Map [String] Int -> [IndentT IO ()]
labels indent n labels
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
    putIndentStrLn indent $ (if percentage < 10 then " " else "") ++ showFFloatAlt (Just 1) percentage "" ++ "% " ++ key

classes :: Int -> Map.Map String Int -> IndentT IO ()
classes n classes = unless (null classes) $ do
  lift (putStr " ")
  parens $ sequence_ (intersperse (lift (putStr ", ")) (map (uncurry (class_ n)) (Map.toList classes)))

class_ :: Int -> String -> Int -> IndentT IO ()
class_ n label n' = let percentage = fromIntegral n' / fromIntegral n * 100 :: Double in lift (putStr (showFFloatAlt (Just 1) percentage ('%':' ':label)))

tables :: Indent -> Int -> Map.Map String (Map.Map String Int) -> IndentT IO ()
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

stat :: Plural -> Int -> Maybe (IndentT IO ())
stat _    0 = Nothing
stat name n = Just . lift $ do
  putStr (show n)
  putStr " "
  putStr (pluralize n name)

tally :: (Int, Int) -> IndentT IO (Int, Int)
tally (successes, failures) = do
  let hasSuccesses = successes /= 0
      hasFailures = failures /= 0
  if hasFailures then
    failure . lift $ putStr "Failed:"
  else
    success . lift $ putStr "Succeeded:"
  lift (putStr " ")
  if hasSuccesses then
    success . lift $ do
      putStr (show successes)
      putStr (if successes == 1 then " success"  else" successes")
  else
    lift (putStr "0 successes")
  lift (putStr ", ")
  if hasFailures then
    failure . lift $ do
      putStr (show failures)
      putStr (if failures == 1 then " failure" else " failures")
  else
    lift (putStr "0 failures")
  newline
  newline
  pure (successes, failures)

setColour :: Color -> SGR
setColour = SetColor Foreground Vivid

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity


parens :: IndentT IO a -> IndentT IO a
parens m = do
  lift (putStr "(")
  a <- m
  a <$ lift (putStr ")")


withSGR :: [SGR] -> IndentT IO a -> IndentT IO a
withSGR sgr io = lift (setSGR sgr) *> io <* lift (setSGR [])

colour :: Color -> IndentT IO a -> IndentT IO a
colour c = withSGR [setColour c]

success, failure :: IndentT IO a -> IndentT IO a

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


newtype IndentT m a = IndentT { runIndentT :: Indent -> m a }

instance Functor m => Functor (IndentT m) where
  fmap f = IndentT . fmap (fmap f) . runIndentT

instance Applicative m => Applicative (IndentT m) where
  pure = lift . pure
  IndentT f <*> IndentT a = IndentT ((<*>) <$> f <*> a)

instance Monad m => Monad (IndentT m) where
  IndentT m >>= f = IndentT (\ i -> do
    a <- m i
    runIndentT (f a) i)

lift :: m a -> IndentT m a
lift = IndentT . const
