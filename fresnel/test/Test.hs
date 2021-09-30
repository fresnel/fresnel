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
import           Data.Foldable (for_, toList, traverse_)
import qualified Data.IntMap as IntMap
import           Data.List (elemIndex, intercalate, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..))
import           Data.Ord (comparing)
import           Data.Semigroup (stimes)
import qualified Fold.Test
import           Fresnel.Lens (Lens', lens)
import           Fresnel.Maybe (_Just)
import           Fresnel.Optional (is)
import           Fresnel.Setter
import           GHC.Exception.Type (Exception(displayException))
import qualified Getter.Test
import qualified Iso.Test
import qualified Monoid.Fork.Test
import           Numeric (readDec, showFFloatAlt)
import qualified Profunctor.Coexp.Test
import qualified Review.Test
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO
import           Test.Group as Group
import           Test.QuickCheck (Args(..), Result(Failure, GaveUp, NoExpectedFailure, Success), isSuccess, quickCheckWithResult, stdArgs, (.&&.), (===))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)
import           Test.QuickCheck.Test (Result(failingClasses))

main :: IO ()
main = getArgs >>= either printErrors (runEntries entries) . parseOpts opts >>= bool exitFailure exitSuccess
  where
  printErrors errs = getProgName >>= traverse_ (hPutStrLn stderr) . errors errs >> pure False
  errors errs name = errs ++ [usageInfo (header name) opts]
  int = fst . head . readDec
  header name = "Usage: " ++ name ++ " [-n N|--successes N]"
  opts =
    [ Option "n" ["successes"] (ReqArg (set (args_.maxSuccess_)        . int) "N") "require N successful tests before concluding the property passes"
    , Option "z" ["size"]      (ReqArg (set (args_.maxSize_)           . int) "N") "increase the size parameter to a maximum of N for successive tests of a property"
    , Option "s" ["shrinks"]   (ReqArg (set (args_.maxShrinks_)        . int) "N") "perform a maximum of N shrinks; setting this to 0 disables shrinking"
    , Option "m" ["match"]     (ReqArg (\ s -> entries_ %~ (s:))           "NAME") "include the named group or property; can be used multiple times to include multiple groups/properties"
    , Option "r" ["replay"]    (ReqArg (set (args_.replay_) . Just . read) "SEED") "the seed and size to repeat"
    ]
  entries =
    [ Fold.Test.tests
    , Getter.Test.tests
    , Iso.Test.tests
    , Monoid.Fork.Test.tests
    , Profunctor.Coexp.Test.tests
    , Review.Test.tests
    , tropical
    ]

parseOpts :: [OptDescr (Options -> Options)] -> [String] -> Either [String] Options
parseOpts opts args
  | null other
  , null errs = Right options
  | otherwise = Left (map ("Unrecognized argument: " ++) other ++ errs)
  where
  options = foldr ($) defaultOptions mods
  (mods, other, errs) = getOpt RequireOrder opts args

runEntries :: [Entry] -> Options -> IO Bool
runEntries groups (Options es args) = runReader stdout (runState (const . pure . not . isFailure) mempty (do
  t <- getAp (foldMap (Ap . runEntry args w) (matching ((==) . entryName) es groups))
  when (hasSuccesses t || hasFailures t) (topIndent end *> runTally t)))
  where
  w = fromMaybe zero (getTropical (maxWidths groups))
  matching _ [] = id
  matching f fs = filter (\ g -> foldr ((||) . f g) False fs)

runEntry :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Args -> Width -> Entry -> m Tally
runEntry args w = \case
  Group name entries -> runGroup args w name entries
  Prop name loc prop -> unit <$> runProp args w name loc prop


maxSuccess_ :: Lens' Args Int
maxSuccess_ = lens maxSuccess (\ a maxSuccess -> a{ maxSuccess })

maxSize_ :: Lens' Args Int
maxSize_ = lens maxSize (\ a maxSize -> a{ maxSize })

maxShrinks_ :: Lens' Args Int
maxShrinks_ = lens maxShrinks (\ a maxShrinks -> a{ maxShrinks })

replay_ :: Lens' Args (Maybe (QCGen, Int))
replay_ = lens replay (\ a replay -> a{ replay })

data Options = Options
  { entries :: [String]
  , args    :: Args
  }

defaultOptions :: Options
defaultOptions = Options{ entries = [], args = stdArgs{ maxSuccess = 250, chatty = False }}

entries_ :: Lens' Options [String]
entries_ = lens entries (\ o entries -> o{ entries })

args_ :: Lens' Options Args
args_ = lens args (\ o args -> o{ args })

runGroup :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Args -> Width -> String -> [Entry] -> m Tally
runGroup args width groupName entries  = do
  topIndent vline
  withSGR [SetConsoleIntensity BoldIntensity] (putS (space ++ groupName) *> nl)
  t <- section Nothing width' (getAp (foldMap Ap (intersperse (mempty <$ blank Nothing) (map (runEntry args width) entries))))
  when (hasSuccesses t || hasFailures t) $ do
    if isFailure t then
      failure' (putS (headingN <> gtally))
    else
      topIndent vline *> putS space
    runTally t
  t <$ topIndent vline <* nl
  where
  width' = width <> stimes (2 :: Int) one

runProp :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Args -> Width -> String -> Loc -> QC.Property -> m Status
runProp args w name Loc{ path, lineNumber } property = withHandle $ \ h ->  do
  isTerminal <- liftIO (hIsTerminalDevice h)

  when isTerminal (title Pass First)

  pos <- gets (bool First Nth . isFailure)
  res <- liftIO (quickCheckWithResult args property)
  let stat' = if isSuccess res then Pass else Fail
  modify (<> unit stat')

  when isTerminal $ do
    liftIO (hClearFromCursorToLineBeginning h)
    liftIO (hSetCursorColumn h 0)

  title stat' pos

  putS "   " *> stat (success (putS "Success")) (failure (putS "Failure")) stat' *> nl

  let stats = resultStats res
      details = numTests stats == maxSuccess args && not (null (classes stats))
      labels = runLabels (Just stat') stats
      body = v_ (Just stat') $ concat
        [ [ line (Just stat') (h_ (runStats args stats ++ runClasses stats)) | details ]
        , do
          Failure{ usedSeed, usedSize, reason, theException, failingTestCase } <- pure res
          pure (do
            line (Just stat') (putS (path ++ ":" ++ show lineNumber))
            line (Just stat') (putS reason)
            for_ theException (line (Just stat') . putS . displayException)
            for_ failingTestCase (line (Just stat') . putS))
            <> [ line (Just stat') (putS ("--replay '(" ++ show usedSeed ++ "," ++ show usedSize ++ ")'")) ]
        , labels
        , runTables stats
        ]

  if details || not (isSuccess res) || not (null labels) then section (Just stat') w body else body
  pure stat'
  where
  title s pos = do
    heading s pos
    withSGR (SetConsoleIntensity BoldIntensity:stat [] [ SetColor Foreground Vivid Red ] s) (putS (bullet ++ name ++ replicate (width w - length name) ' '))
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

runStats :: (Has (Reader Handle) sig m, MonadIO m) => Args -> Stats -> [m ()]
runStats Args{ maxSuccess } Stats{ numTests, numDiscarded, numShrinks } = [ sepBy_ (putS ", ") entries *> putS "." | not (null entries) ]
  where
  entries = concat
    [ [ putS (show numTests ++ " test" ++ singular numTests) | numTests > 0 && numTests /= maxSuccess ]
    , [ putS (show numDiscarded ++ " discarded") | numDiscarded > 0 ]
    , [ putS (show numShrinks ++ " shrink" ++ singular numShrinks) | numShrinks > 0 ]
    ]


runLabels :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> Stats -> [m ()]
runLabels s Stats{ numTests, labels }
  | null labels = []
  | otherwise   = IntMap.elems numberedLabels >>= param
  where
  numberedLabels = IntMap.fromListWith (Map.unionWith (+)) $
    [ (i, Map.singleton l n)
    | (labels, n) <- Map.toList labels
    , (i, l) <- zip [(0 :: Int)..] labels
    ]
  param m =
    [ for_ (zip [1..] scaled) $ \ (i, (key, v)) ->
        line s $ putS (show (i :: Int) ++ ". " ++  (' ' <$ guard (v < 10)) ++ showFFloatAlt (Just 1) v "" ++ "% " ++ key)
    , do
      line s $ putS [ c | e <- sparked, c <- [e, e, e] ]
      line s $ putS [ c | k <- Map.keys m, i <- maybe [] (pure . succ) (elemIndex k (map fst sorted)), c <- ' ':show i ++ " " ]
    ]
    where
    n = realToFrac numTests :: Double
    sorted = sortBy (flip (comparing snd) <> flip (comparing fst)) (Map.toList m)
    scaled = map (fmap (\ v -> realToFrac v / n * 100)) sorted
    sparked = sparkify (map snd (Map.toList m))

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
runTally t = do
  sepBy_ (putS ", " )
    $  [ success (formatTally "✓" "success" "successes" (successes t)) | hasSuccesses t ]
    ++ [ failure (formatTally "✗" "failure" "failures"  (failures t))  | hasFailures  t ]
  putS "." <* nl
  where
  formatTally prefix s p n = h_ [ putS prefix, putS (show n), putS (plural n s p) ]


sepBy_ :: (Applicative m, Monoid a) => m a -> [m a] -> m a
sepBy_ sep = getAp . foldMap Ap . intersperse sep

h_ :: (Has (Reader Handle) sig m, MonadIO m) => [m ()] -> m ()
h_ = sepBy_ (putS " ")

v_ :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> [m ()] -> m ()
v_ = sepBy_ . blank


isFailure :: Tally -> Bool
isFailure = (/= 0) . failures

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

tropical :: Entry
tropical = Group.Group
  "Test.Group.Tropical"
  [ semigroupAssoc
  , monoidIdentity
  ]
  where
  semigroupAssoc = Group.Prop"semigroup assoc" here $ QC.property (\ (ArbTropical a) (ArbTropical b) (ArbTropical c) -> a <> (b <> c) === (a <> b) <> c)
  monoidIdentity = Group.Prop"monoid identity" here $ QC.property (\ (ArbTropical a) -> (mempty <> a) === a .&&. (a <> mempty) === a)

newtype ArbTropical = ArbTropical (Tropical Int)
  deriving (Eq, Ord, Show)

instance QC.Arbitrary ArbTropical where
  arbitrary = QC.oneof $ map (fmap (ArbTropical . Tropical))
    [ pure Nothing
    , Just <$> QC.arbitrary
    ]


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

data Pos = First | Nth


topIndent :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => String -> m ()
topIndent m = gets isFailure >>= bool (putS space) (failure' (putS m))

withHandle :: Has (Reader Handle) sig m => (Handle -> m a) -> m a
withHandle = join . asks


blank :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> m ()
blank s = line s (pure ())

heading :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Status -> Pos -> m ()
heading st p = case st of
  Pass -> topIndent vline             *> putS vline
  Fail -> topIndent (headingGutter p) *> failure' (putS (group First ++ arrow))


line :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> m a -> m a
line st m = do
  topIndent vline
  putS vline
  when (is _Just st) (status st (putS vline))
  m <* nl

section :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> Width -> m a -> m a
section s w m = rule '╭' *> m <* rule '╰'
  where
  rule corner = indent *> status s (putS (corner : h : replicate fullWidth h)) *> nl
  indent = topIndent vline *> when (is _Just s) (putS vline)
  h = '─'
  fullWidth = width w + 3 + length "Success"


space, bullet, heading1, headingN, arrow, vline, hline, gtally, end, vlineR :: String
space    = "  "
bullet   = "☙ "
heading1 = "╭─"
headingN = "├─"
arrow    = "▶ "
vline    = "│ "
hline    = "──"
gtally   = "┤ "
end      = "╰─┤ "
vlineR   = "├─"

group :: Pos -> String
group = \case
  First -> hline
  Nth   -> vlineR

headingGutter :: Pos -> String
headingGutter = \case
  First -> heading1
  Nth   -> headingN


nl :: (Has (Reader Handle) sig m, MonadIO m) => m ()
nl = withHandle (liftIO . (`hPutStrLn` ""))

putS :: (Has (Reader Handle) sig m, MonadIO m) => String -> m ()
putS s = withHandle (liftIO . (`hPutStr` s))
