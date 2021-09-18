{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main
( main
) where

import           Control.Monad (unless, when)
import           Data.Foldable (fold, for_, toList)
import qualified Data.IntMap as IntMap
import           Data.List (intersperse, sortBy)
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
import           Test.QuickCheck (Args(..), Result(Failure, GaveUp, NoExpectedFailure, Success), isSuccess, quickCheckWithResult, stdArgs, (.&&.), (===))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)
import           Test.QuickCheck.Test (Result(failingClasses))

main :: IO ()
main = do
  let opts =
        [ Option "n" ["successes"] (ReqArg (set (args_.maxSuccess_)        . int) "N") "require N successful tests before concluding the property passes"
        , Option "z" ["size"]      (ReqArg (set (args_.maxSize_)           . int) "N") "increase the size parameter to a maximum of N for successive tests of a property"
        , Option "s" ["shrinks"]   (ReqArg (set (args_.maxShrinks_)        . int) "N") "perform a maximum of N shrinks; setting this to 0 disables shrinking"
        , Option "g" ["group"]     (ReqArg (\ s -> groups_ %~ (s:))            "NAME") "include the named group; can be used multiple times to include multiple groups"
        , Option "r" ["replay"]    (ReqArg (set (args_.replay_) . Just . read) "SEED") "the seed and size to repeat"
        ]
      i = putStr "  "
  (mods, other, errs) <- getOpt RequireOrder opts <$> getArgs
  case map ("Unrecognized argument: " ++) other ++ errs of
    [] -> pure ()
    _  -> do
      name <- getProgName
      for_ (errs ++ [usageInfo (header name) opts]) (hPutStrLn stderr)
  let Options gs _ args = foldr ($) (Options{ groups = [], cases = [], args = stdArgs{ maxSuccess = 250, chatty = False }}) mods
      matching _ [] = id
      matching f fs = filter (\ g -> foldr ((||) . f g) False fs)
  (_, failures) <- (`runLayout` i) $ do
    res <- traverse (runGroup args w) (matching ((==) . groupName) gs groups)
    uncurry tally (foldr (\ (s, f) (ss, fs) -> (s + ss, f + fs)) (0, 0) res)
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

runGroup :: Args -> Int -> Group -> Layout (Int, Int)
runGroup args width Group{ groupName, cases } = do
  withSGR [setBold] $ lineStr groupName
  lineStr (replicate (2 + fullWidth width) '━')
  rs <- catMaybes <$> sequence (intersperse (Nothing <$ putLn "") (map (fmap Just <$> runCase args width) cases))
  putLn ""
  tally (length (filter id rs)) (length (filter not rs))

runCase :: Args -> Int -> Case -> Layout Bool
runCase args width Case{ name, loc = Loc{ path, lineNumber }, property } = do
  lift saveCursor
  title

  res <- lift (quickCheckWithResult args property)
  let succeeded f t
        | isSuccess res = success t
        | otherwise     = failure f

  succeeded (lift restoreCursor *> failure title) (pure ())

  put "   " *> succeeded (putLn "Failure") (putLn "Success")

  let stats = resultStats res
      details = not (null (fold (Map.keys (labels stats))) && null (classes stats) && null (tables stats))

  when details $ incr (succeeded (put "╭─") (put "  ")) $ line $ withSGR [SetColor Foreground Vivid (if isSuccess res then Green else Red)] (putLn (replicate (fullWidth width) '─'))

  incr (succeeded (failure (put "│ ")) (put "  ")) $ paras $ concat
    [ [ runStats stats *> runClasses stats *> putLn "." | details ]
    , case res of
      Failure{ usedSeed, usedSize, reason, theException, failingTestCase } ->
        [ do
          lineStr (path ++ ":" ++ show lineNumber)
          lineStr reason
          for_ theException (lineStr . displayException)
          for_ failingTestCase lineStr
        , lineStr ("--replay (" ++ show usedSeed ++ "," ++ show usedSize ++ ")")
        ]
      _ -> []
    , runLabels stats
    , runTables stats
    ]
  pure (isSuccess res)
  where
  δ = width - length name
  title = line $ do
    _ <- withSGR [setBold] (put ("❧ " ++ name ++ replicate δ ' '))
    lift (hFlush stdout)
  paras = sequence_ . intersperse (lineStr "")

data Stats = Stats
  { numTests     :: Int
  , numDiscarded :: Int
  , numShrinks   :: Int
  , labels       :: Map.Map [String] Int
  , classes      :: Map.Map String Int
  , tables       :: Map.Map String (Map.Map String Int)
  }

defaultStats :: Stats
defaultStats = Stats
  { numTests     = 0
  , numDiscarded = 0
  , numShrinks   = 0
  , labels       = Map.empty
  , classes      = Map.empty
  , tables       = Map.empty
  }

resultStats :: Result -> Stats
resultStats = \case
  Success{ numTests, numDiscarded, labels, classes, tables }                   -> defaultStats{ numTests, numDiscarded, labels, classes, tables }
  GaveUp{ numTests, numDiscarded, labels, classes, tables }                    -> defaultStats{ numTests, numDiscarded, labels, classes, tables }
  Failure{ numTests, numDiscarded, numShrinks, failingLabels, failingClasses } -> defaultStats{ numTests, numDiscarded, numShrinks, labels = Map.fromList (map ((, numTests) . pure) failingLabels), classes = Map.fromList (map (,numTests) (toList failingClasses)) }
  NoExpectedFailure{ numTests, numDiscarded, labels, classes, tables }         -> defaultStats{ numTests, numDiscarded, labels, classes, tables }

runStats :: Stats -> Layout ()
runStats Stats{ numTests, numDiscarded, numShrinks } = line . sequence_ . intersperse (put ", ")
  $  toList (stat (S "test") numTests)
  ++ toList (stat (S "discard") numDiscarded)
  ++ toList (stat (S "shrink") numShrinks)


runLabels :: Stats -> [Layout ()]
runLabels Stats{ numTests = n, labels }
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
    lineStr $ (if percentage < 10 then " " else "") ++ showFFloatAlt (Just 1) percentage "" ++ "% " ++ key

runClasses :: Stats -> Layout ()
runClasses Stats{ numTests = n, classes } = unless (null classes) $ do
  put " "
  parens $ sequence_ (intersperse (put ", ") (map (uncurry (class_ n)) (Map.toList classes)))

class_ :: Int -> String -> Int -> Layout ()
class_ n label n' = put $ if n == n' then label else showFFloatAlt (Just 1) (fromIntegral n' / fromIntegral n * 100 :: Double) ('%':' ':label)

runTables :: Stats -> [Layout ()]
runTables _ = []


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
fullWidth width = width + 3 + length "Success"

stat :: Plural -> Int -> Maybe (Layout ())
stat _    0 = Nothing
stat name n = Just $ do
  put (show n)
  put " "
  put (pluralize n name)

tally :: Int -> Int -> Layout (Int, Int)
tally successes failures = ((successes, failures) <$) . line $ do
  let hasSuccesses = successes /= 0
      hasFailures = failures /= 0
  when hasSuccesses . success $ do
    put (show successes)
    put (' ' : pluralize successes (C "success" "successes"))
  when (hasSuccesses && hasFailures) $ put ", "
  when hasFailures . failure $ do
    put (show failures)
    put (' ' : pluralize failures (S "failure"))
  when (hasSuccesses || hasFailures) (putLn ".")
  putLn ""

setColour :: Color -> SGR
setColour = SetColor Foreground Vivid

setBold :: SGR
setBold = SetConsoleIntensity BoldIntensity


parens :: Layout a -> Layout a
parens m = put "(" *> m <* put ")"


withSGR :: [SGR] -> Layout a -> Layout a
withSGR sgr io = lift (setSGR sgr) *> io <* lift (setSGR [])

colour :: Color -> Layout a -> Layout a
colour c = withSGR [setColour c]

success, failure :: Layout a -> Layout a

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


newtype Layout a = Layout { runLayout :: IO () -> IO a }

instance Functor Layout where
  fmap f = Layout . (fmap f .) . runLayout

instance Applicative Layout where
  pure = lift . pure
  Layout f <*> Layout a = Layout ((<*>) <$> f <*> a)

instance Monad Layout where
  Layout m >>= f = Layout (\ i -> m i >>= (`runLayout` i) . f)

lift :: IO a -> Layout a
lift = Layout . const

incr :: Layout () -> Layout a -> Layout a
incr j m = Layout (\ i -> runLayout m (i *> runLayout j i))

line :: Layout a -> Layout a
line m = Layout (\ i -> i *> runLayout m i)

lineStr :: String -> Layout ()
lineStr s = line $ putLn s

put :: String -> Layout ()
put = lift . putStr

putLn :: String -> Layout ()
putLn = lift . putStrLn
