{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main
( main
) where

import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Monad (guard, join, when)
import           Control.Monad.IO.Class
import           Data.Bool (bool)
import           Data.Foldable (for_, toList)
import qualified Data.IntMap as IntMap
import           Data.List (elemIndex, intercalate, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..))
import           Data.Ord (comparing)
import qualified Fold.Test
import           Fresnel.Maybe (_Just)
import           Fresnel.Optional (is)
import           Fresnel.Tropical
import           GHC.Exception.Type (Exception(displayException))
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import           Numeric (showFFloatAlt)
import qualified Profunctor.Coexp.Test
import qualified Review.Test
import           System.Console.ANSI
import           System.Environment (getArgs)
import           System.IO
import           Test.Group as Group
import           Test.Options
import           Test.QuickCheck (Args(..), Result(Failure, GaveUp, NoExpectedFailure, Success), isSuccess, quickCheckWithResult)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Test (Result(failingClasses))
import qualified Tropical.Test

main :: IO ()
main = getArgs >>= withOptions defaultOpts (runEntries entries)
  where
  entries =
    [ Fold.Test.tests
    , Getter.Test.tests
    , Iso.Test.tests
    , Monoid.Fork.Test.tests
    , Profunctor.Coexp.Test.tests
    , Review.Test.tests
    , Tropical.Test.tests
    ]

runEntries :: [Entry] -> Options -> IO Bool
runEntries groups (Options es args) = runReader w . runReader args . runReader stdout . runState (const . pure . not . hasFailures) mempty $ do
  t <- getAp (foldMap Ap (intersperse (mempty <$ blank <* blank) (map runEntry (matching ((==) . entryName) es groups))))
  when (hasSuccesses t || hasFailures t) (blank *> topIndent end *> runTally t)
  where
  blank = topIndent vline <* nl
  w = fromMaybe zero (getTropical (maxWidths groups))
  matching _ [] = id
  matching f fs = filter (\ g -> foldr ((||) . f g) False fs)

runEntry :: (Has (Reader Args) sig m, Has (Reader Handle) sig m, Has (Reader Width) sig m, Has (State Tally) sig m, MonadIO m) => Entry -> m Tally
runEntry = \case
  Group name entries -> runGroup name entries
  Prop name loc prop -> runProp  name loc prop


runGroup :: (Has (Reader Args) sig m, Has (Reader Handle) sig m, Has (Reader Width) sig m, Has (State Tally) sig m, MonadIO m) => String -> [Entry] -> m Tally
runGroup groupName entries  = do
  topIndent vline
  withSGR [SetConsoleIntensity BoldIntensity] (putS (space ++ groupName) *> nl)
  w <- ask
  t <- local (<> Width 2) (section Nothing (getAp (foldMap Ap (intersperse (mempty <$ line <* nl) (map (local (const (w :: Width)) . runEntry) entries)))))
  when (hasSuccesses t || hasFailures t) $ do
    if hasFailures t then
      failure' (putS (headingN <> gtally))
    else
      topIndent vline *> putS space
    runTally t
  pure t

runProp :: (Has (Reader Args) sig m, Has (Reader Handle) sig m, Has (Reader Width) sig m, Has (State Tally) sig m, MonadIO m) => String -> Loc -> QC.Property -> m Tally
runProp name loc property = runPropWith (ask >>= \ args -> liftIO (quickCheckWithResult args property)) name loc

runPropWith :: (Has (Reader Args) sig m, Has (Reader Handle) sig m, Has (Reader Width) sig m, Has (State Tally) sig m, MonadIO m) => m Result -> String -> Loc -> m Tally
runPropWith run name Loc{ path, lineNumber } = withHandle $ \ h ->  do
  isTerminal <- liftIO (hIsTerminalDevice h)

  when isTerminal (title Pass False)

  failedPreviously <- gets hasFailures
  res <- run
  let stat' = if isSuccess res then Pass else Fail
  modify (<> unit stat')

  when isTerminal $ do
    liftIO (hClearFromCursorToLineBeginning h)
    liftIO (hSetCursorColumn h 0)

  title stat' failedPreviously

  putS "   " *> stat (success (putS "Success")) (failure (putS "Failure")) stat' *> nl

  maxSuccess <- asks maxSuccess
  let stats = resultStats res
      details = numTests stats == maxSuccess && not (null (classes stats))
      labels = runLabels stat' stats
      ln b = line *> status (Just stat') (putS vline) *> b *> nl
      body = sepBy_ (ln (pure ())) $ concat
        [ [ ln (sepBy_ (putS " ") (runStats maxSuccess stats ++ runClasses stats)) | details ]
        , do
          Failure{ usedSeed, usedSize, reason, theException, failingTestCase } <- pure res
          pure (do
            ln (putS (path ++ ":" ++ show lineNumber))
            ln (putS reason)
            for_ theException (ln . putS . displayException)
            for_ failingTestCase (ln . putS))
            <> [ ln (putS ("--replay '(" ++ show usedSeed ++ "," ++ show usedSize ++ ")'")) ]
        , labels
        , runTables stats
        ]

  if details || not (isSuccess res) || not (null labels) then section (Just stat') body else body
  pure (unit stat')
  where
  title s failedPreviously = do
    topIndent (stat vline (bool heading1 headingN failedPreviously) s)
    stat (putS vline) (failure' (putS (hline ++ arrow))) s
    w <- asks width
    withSGR (SetConsoleIntensity BoldIntensity:stat [] [ SetColor Foreground Vivid Red ] s) (putS (bullet ++ name ++ replicate (w - length name) ' '))
    withHandle (liftIO . hFlush)

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

runStats :: (Has (Reader Handle) sig m, MonadIO m) => Int -> Stats -> [m ()]
runStats maxSuccess Stats{ numTests, numDiscarded, numShrinks } = [ sepBy_ (putS ", ") entries *> putS "." | not (null entries) ]
  where
  entries = concat
    [ [ putS (show numTests     ++ " test"   ++ singular numTests)   | numTests     > 0, numTests /= maxSuccess ]
    , [ putS (show numDiscarded ++ " discarded")                     | numDiscarded > 0 ]
    , [ putS (show numShrinks   ++ " shrink" ++ singular numShrinks) | numShrinks   > 0 ]
    ]


runLabels :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Status -> Stats -> [m ()]
runLabels s Stats{ numTests, labels }
  | null labels = []
  | otherwise   = IntMap.elems numberedLabels >>= \ m ->
    let sorted = sortBy (flip (comparing snd) <> flip (comparing fst)) (Map.toList m)
        scaled = map (fmap (\ v -> realToFrac v / n * 100)) sorted
        sparked = sparkify (Map.elems m)
        ln l = line *> status (Just s) (putS vline) *> putS l *> nl
    in
    [ for_ (zip [1..] scaled) $ \ (i, (key, v)) -> ln (show (i :: Int) ++ ". " ++  (' ' <$ guard (v < 10)) ++ showFFloatAlt (Just 1) v "" ++ "% " ++ key)
    , do
      ln [ c | e <- sparked, c <- [e, e, e] ]
      ln [ c | k <- Map.keys m, i <- maybe [] (pure . succ) (elemIndex k (map fst sorted)), c <- ' ':show i ++ " " ] ]
  where
  numberedLabels = IntMap.fromListWith (Map.unionWith (+))
    [ (i, Map.singleton l n)
    | (labels, n) <- Map.toList labels
    , (i, l) <- zip [(0 :: Int)..] labels ]
  n = realToFrac numTests :: Double

runClasses :: (Has (Reader Handle) sig m, MonadIO m) => Stats -> [m ()]
runClasses Stats{ numTests = n, classes } = [ putS (intercalate ", " (map (uncurry (class_ n)) (Map.toList classes)) ++ ".") | not (null classes) ] where
  class_ n label n' = if n == n' then label else showFFloatAlt (Just 1) (fromIntegral n' / fromIntegral n * 100 :: Double) ('%':' ':label)

runTables :: Stats -> [m ()]
runTables _ = []


plural :: Int -> a -> a -> a
plural 1 s _ = s
plural _ _ p = p

singular :: Int -> String
singular 1 = "s"
singular _ = ""

runTally :: (Has (Reader Handle) sig m, MonadIO m) => Tally -> m ()
runTally t = sepBy_ (putS ", " ) (map success (tally "✓" "success" "successes" (successes t)) ++ map failure (tally "✗" "failure" "failures"  (failures t))) *> putS "." *> nl
  where
  tally prefix s p n = [ sepBy_ (putS " ") [ putS prefix, putS (show n), putS (plural n s p) ] | n /= 0 ]


sepBy_ :: Applicative m => m () -> [m ()] -> m ()
sepBy_ sep = getAp . foldMap Ap . intersperse sep


hasSuccesses :: Tally -> Bool
hasSuccesses = (/= 0) . successes

hasFailures :: Tally -> Bool
hasFailures = (/= 0) . failures

unit :: Status -> Tally
unit = stat (Tally 1 0) (Tally 0 1)

data Tally = Tally { successes :: Int, failures :: Int }

instance Semigroup Tally where
  Tally s1 f1 <> Tally s2 f2 = Tally (s1 + s2) (f1 + f2)

instance Monoid Tally where
  mempty = Tally 0 0


withSGR :: (Has (Reader Handle) sig m, MonadIO m) => [SGR] -> m a -> m a
withSGR sgr m = withHandle $ \ h -> liftIO (hSetSGR h sgr) *> m <* liftIO (hSetSGR h [])

colour :: (Has (Reader Handle) sig m, MonadIO m) => ColorIntensity -> Color -> m a -> m a
colour i c = withSGR [SetColor Foreground i c]

success, failure, failure' :: (Has (Reader Handle) sig m, MonadIO m) => m a -> m a

success = colour Vivid Green
failure = colour Vivid Red
failure' = colour Dull Red

status :: (Has (Reader Handle) sig m, MonadIO m) => Maybe Status -> m a -> m a
status = maybe id (stat success failure)


sparks :: String
sparks = " ▁▂▃▄▅▆▇█"

sparkify :: Real a => [a] -> String
sparkify bins = sparkifyRelativeTo sparks (maximum bins) bins

sparkifyRelativeTo :: Real a => String -> a -> [a] -> String
sparkifyRelativeTo sparks max = fmap spark
  where
  spark n = sparks !! round (realToFrac n / realToFrac max * realToFrac (length sparks - 1) :: Double)


stat :: a -> a -> Status -> a
stat pass fail = \case{ Pass -> pass ; Fail -> fail }

data Status = Pass | Fail


topIndent :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => String -> m ()
topIndent m = gets hasFailures >>= failure' . putS . bool space m

withHandle :: Has (Reader Handle) sig m => (Handle -> m a) -> m a
withHandle = join . asks


line :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => m ()
line = topIndent vline *> putS vline

section :: (Has (Reader Handle) sig m, Has (Reader Width) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> m a -> m a
section s m = do
  fullWidth <- asks ((+ (3 + length "Success")) . width)
  let rule corner = indent *> status s (putS (corner : h : replicate fullWidth h)) *> nl
  rule '╭' *> m <* rule '╰'
  where
  indent = topIndent vline *> when (is _Just s) (putS vline)
  h = '─'


space, bullet, heading1, headingN, arrow, vline, hline, gtally, end :: String
space    = "  "
bullet   = "☙ "
heading1 = "╭─"
headingN = "├─"
arrow    = "▶ "
vline    = "│ "
hline    = "──"
gtally   = "┤ "
end      = "╰─┤ "


nl :: (Has (Reader Handle) sig m, MonadIO m) => m ()
nl = withHandle (liftIO . (`hPutStrLn` ""))

putS :: (Has (Reader Handle) sig m, MonadIO m) => String -> m ()
putS s = withHandle (liftIO . (`hPutStr` s))
