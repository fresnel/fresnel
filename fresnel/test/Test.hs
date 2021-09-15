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
  res <- traverse (runGroup stdArgs{ maxSuccess = 250, chatty = False } width) groups
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

data Loc = Loc { path :: FilePath, lineNumber :: Int }

mkCase :: String -> Property -> Case
mkCase s property = Case{ name, loc = Loc{ path, lineNumber }, property }
  where
  (name, path, lineNumber) = case breaks [isSpace, not . isSpace, isSpace, not . isSpace, (== ':'), (/= ':')] s of
    [n, _, _, _, p, _, l] -> (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') n)), p, fst (head (readDec l)))
    _                     -> ("", "", 0)

newtype Indent = Indent { getIndent :: [String] }

initialIndent :: Indent
initialIndent = Indent []

push :: String -> Indent -> Indent
push f (Indent fs) = Indent (f:fs)

put :: String -> IndentT IO ()
put = lift . putStr

putNewline :: String -> IndentT IO ()
putNewline s = put s *> newline

line :: IndentT IO a -> IndentT IO a
line m = do
  i <- asks (concat . reverse . getIndent)
  put i
  m

newline :: IndentT IO ()
newline = lift (putStrLn "")

runGroup :: Args -> Int -> Group -> IndentT IO (Int, Int)
runGroup args width Group{ groupName, cases } = do
  withSGR [setBold] $ putNewline groupName
  putNewline (replicate (2 + fullWidth width) '━')
  rs <- catMaybes <$> local (push "  ") (sequence (intersperse (Nothing <$ newline) (map (fmap Just <$> runCase args width) cases)))
  newline
  tally (length (filter id rs), length (filter not rs))

runCase :: Args -> Int -> Case -> IndentT IO Bool
runCase args width Case{ name, loc, property } = do
  r <- lift (quickCheckWithResult args property)
  result width name loc r
  pure (isSuccess r)

result :: Int -> String -> Loc -> Result -> IndentT IO ()
result width name Loc{ path } = \case
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
    stats Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks }
    unless (null failingClasses) $ put (" (" ++ intercalate ", " (toList failingClasses) ++ ")")
    putNewline ":"
    line $ putNewline path
    line $ putNewline reason
    for_ theException (line . putNewline . displayException)
    for_ failingTestCase (line . putNewline)
    line $ putNewline ""
    line $ putNewline ("Seed: " ++ show usedSeed)
    line $ putNewline ("Size: " ++ show usedSize)
    unless (null failingLabels) . line . putNewline $ "Labels: "  ++ intercalate ", " failingLabels

  NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables } -> do
    header False
    Main.classes numTests classes
    body numTests labels tables Stats{ Main.numTests, Main.numDiscarded, Main.numShrinks = 0 } ":"
  where
  header succeeded = do
    put "❧ "
    let δ = width - length name
    withSGR [setBold] (put name *> when (width > 0) (put (replicate δ ' ')))
    put "   "
    if succeeded then
      success $ putNewline "Success."
    else
      failure $ putNewline "Failure."

    line $ withSGR [setColour (if succeeded then Green else Red)] $ putNewline (replicate (fullWidth width) '─')

  body numTests labels tables s t = do
    sequence_ (intersperse (line (putNewline "")) ((stats s *> putNewline t) : Main.labels numTests labels))
    Main.tables numTests tables

data Stats = Stats
  { numTests     :: Int
  , numDiscarded :: Int
  , numShrinks   :: Int
  }

stats :: Stats -> IndentT IO ()
stats Stats{ numTests, numDiscarded, numShrinks } = line . sequence_ . intersperse (put ", ")
  $  toList (stat (S "test") numTests)
  ++ toList (stat (S "discard") numDiscarded)
  ++ toList (stat (S "shrink") numShrinks)


labels :: Int -> Map.Map [String] Int -> [IndentT IO ()]
labels n labels
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
    line . putNewline $ (if percentage < 10 then " " else "") ++ showFFloatAlt (Just 1) percentage "" ++ "% " ++ key

classes :: Int -> Map.Map String Int -> IndentT IO ()
classes n classes = unless (null classes) $ do
  put " "
  parens $ sequence_ (intersperse (put ", ") (map (uncurry (class_ n)) (Map.toList classes)))

class_ :: Int -> String -> Int -> IndentT IO ()
class_ n label n' = let percentage = fromIntegral n' / fromIntegral n * 100 :: Double in put (showFFloatAlt (Just 1) percentage ('%':' ':label))

tables :: Int -> Map.Map String (Map.Map String Int) -> IndentT IO ()
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

fullWidth :: Int -> Int
fullWidth width = width + 3 + length "Success."

stat :: Plural -> Int -> Maybe (IndentT IO ())
stat _    0 = Nothing
stat name n = Just $ do
  put (show n)
  put " "
  put (pluralize n name)

tally :: (Int, Int) -> IndentT IO (Int, Int)
tally (successes, failures) = withSGR [setBold] $ do
  let hasSuccesses = successes /= 0
      hasFailures = failures /= 0
  when hasSuccesses . success $ do
    put (show successes)
    put (' ' : pluralize successes (C "success" "successes"))
  when (hasSuccesses && hasFailures) $ put ", "
  when hasFailures . failure $ do
    put (show failures)
    put (' ' : pluralize failures (S "failure"))
  when (hasSuccesses || hasFailures) (putNewline ".")
  newline
  pure (successes, failures)

setColour :: Color -> SGR
setColour = SetColor Foreground Vivid

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity


parens :: IndentT IO a -> IndentT IO a
parens m = do
  put "("
  a <- m
  a <$ put ")"


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

asks :: Applicative m => (Indent -> a) -> IndentT m a
asks = IndentT . fmap pure

local :: (Indent -> Indent) -> (IndentT m a -> IndentT m a)
local f m = IndentT (runIndentT m . f)
