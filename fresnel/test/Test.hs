{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main
( main
) where

import           Control.Monad (guard, unless, when)
import           Control.Monad.IO.Class
import           Data.Bool (bool)
import           Data.Foldable (for_, toList, traverse_)
import           Data.Function ((&))
import qualified Data.IntMap as IntMap
import           Data.List (elemIndex, intercalate, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup (stimes)
import qualified Fold.Test
import           Fresnel.Fold ((^?))
import           Fresnel.Lens (Lens', lens)
import           Fresnel.Maybe (_Just)
import           Fresnel.Prism (Prism', is, prism')
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
import           Test.Group as Group
import           Test.QuickCheck (Args(..), Result(Failure, GaveUp, NoExpectedFailure, Success), isSuccess, quickCheckWithResult, stdArgs, (.&&.), (===))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)
import           Test.QuickCheck.Test (Result(failingClasses))

main :: IO ()
main = getArgs >>= either printErrors (run groups) . parseOpts opts >>= bool exitFailure exitSuccess
  where
  printErrors errs = getProgName >>= traverse_ (hPutStrLn stderr) . errors errs >> pure False
  errors errs name = errs ++ [usageInfo (header name) opts]
  int = fst . head . readDec
  header name = "Usage: " ++ name ++ " [-n N|--successes N]"
  opts =
    [ Option "n" ["successes"] (ReqArg (set (args_.maxSuccess_)        . int) "N") "require N successful tests before concluding the property passes"
    , Option "z" ["size"]      (ReqArg (set (args_.maxSize_)           . int) "N") "increase the size parameter to a maximum of N for successive tests of a property"
    , Option "s" ["shrinks"]   (ReqArg (set (args_.maxShrinks_)        . int) "N") "perform a maximum of N shrinks; setting this to 0 disables shrinking"
    , Option "g" ["group"]     (ReqArg (\ s -> groups_ %~ (s:))            "NAME") "include the named group; can be used multiple times to include multiple groups"
    , Option "r" ["replay"]    (ReqArg (set (args_.replay_) . Just . read) "SEED") "the seed and size to repeat"
    ]
  groups =
    [ Fold.Test.tests
    , Getter.Test.tests
    , Iso.Test.tests
    , Monoid.Fork.Test.tests
    , Profunctor.Coexp.Test.tests
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

run :: [Group] -> Options -> IO Bool
run groups (Options gs _ args) = not . isFailure . tally <$> runLayout (do
  (t, _) <- listen (traverse_ (runGroup args w) (matching ((==) . groupName) gs groups))
  sequence_ (runTally t)) (const pure) stdout (State Pass Nothing Nothing mempty)
  where
  w = fromMaybe zero (getTropical (maxWidths groups))
  matching _ [] = id
  matching f fs = filter (\ g -> foldr ((||) . f g) False fs)


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

defaultOptions :: Options
defaultOptions = Options{ groups = [], cases = [], args = stdArgs{ maxSuccess = 250, chatty = False }}

groups_ :: Lens' Options [String]
groups_ = lens groups (\ o groups -> o{ groups })

args_ :: Lens' Options Args
args_ = lens args (\ o args -> o{ args })

runGroup :: Args -> Width -> Group -> Layout ()
runGroup args width Group.Group{ groupName, cases } = do
  bookend groupStatus_ Pass $ do
    heading $ withSGR [SetConsoleIntensity BoldIntensity] $ put groupName *> nl
    (t, _) <- sandwich True width $ listen $ sequence_ (intersperse (line (pure ())) . (`map` cases) $ \ c -> do
      succeeded <- bookend caseStatus_ Pass (runCase args width c)
      unless succeeded $ groupStatus_ %= Just . Fail . maybe First (stat First (const Nth)))
    sequence_ (runTally t)
  line (pure ())

bookend :: Setter State State a (Maybe b) -> b -> Layout c -> Layout c
bookend o v m = o ?= v *> m <* o .= Nothing

sandwich :: Bool -> Width -> Layout a -> Layout a
sandwich cond w m = when cond (bar Top) *> m <* when cond (bar Bottom) where
  bar side = rule side (w <> stimes (2 :: Int) one)

runCase :: Args -> Width -> Case -> Layout Bool
runCase args w Group.Case{ name, loc = Loc{ path, lineNumber }, property } = do
  title False

  res <- liftIO (quickCheckWithResult args property)
  tell (fromBool (isSuccess res))
  unless (isSuccess res) $ do
    groupStatus_ %= Just . Fail . maybe First (stat First (const Nth))
    topStatus_ %= Fail . stat First (const Nth)
  let stat = if isSuccess res then Pass else Fail Nth
  caseStatus_ ?= stat

  unless (isSuccess res) $ do
    withHandle (liftIO . hClearFromCursorToLineBeginning)
    withHandle (liftIO . (`hSetCursorColumn` 0))
    failure (title True)

  put "   " *> status (Just stat) (put (bool "Failure" "Success" (isSuccess res))) *> nl

  let stats = resultStats res
      details = numTests stats == maxSuccess args && not (null (classes stats))
      labels = runLabels stats

  sandwich (details || not (isSuccess res) || not (null labels)) w $ v_ $ concat
    [ [ line (h_ (runStats args stats ++ runClasses stats)) | details ]
    , do
      Failure{ usedSeed, usedSize, reason, theException, failingTestCase } <- pure res
      [ do
        line (put (path ++ ":" ++ show lineNumber))
        line (put reason)
        for_ theException (line . put . displayException)
        for_ failingTestCase (line . put)
        , line (put ("--replay '(" ++ show usedSeed ++ "," ++ show usedSize ++ ")'")) ]
    , labels
    , runTables stats
    ]
  pure $! isSuccess res
  where
  title failed = heading $ do
    withSGR (SetConsoleIntensity BoldIntensity:[ SetColor Foreground Vivid Red | failed ]) (put (name ++ replicate (width w - length name) ' '))
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

runStats :: Args -> Stats -> [Layout ()]
runStats Args{ maxSuccess } Stats{ numTests, numDiscarded, numShrinks } = [ sepBy_ (put ", ") entries *> put "." | not (null entries) ]
  where
  entries = concat
    [ [ put (show numTests ++ " test" ++ singular numTests) | numTests > 0 && numTests /= maxSuccess ]
    , [ put (show numDiscarded ++ " discarded") | numDiscarded > 0 ]
    , [ put (show numShrinks ++ " shrink" ++ singular numShrinks) | numShrinks > 0 ]
    ]


runLabels :: Stats -> [Layout ()]
runLabels Stats{ numTests, labels }
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
        line $ put (show (i :: Int) ++ ". " ++  (' ' <$ guard (v < 10)) ++ showFFloatAlt (Just 1) v "" ++ "% " ++ key)
    , do
      line $ put [ c | e <- sparked, c <- [e, e, e] ]
      line $ put [ c | k <- Map.keys m, i <- maybe [] (pure . succ) (elemIndex k (map fst sorted)), c <- ' ':show i ++ " " ]
    ]
    where
    n = realToFrac numTests :: Double
    sorted = sortBy (flip (comparing snd) <> flip (comparing fst)) (Map.toList m)
    scaled = map (fmap (\ v -> realToFrac v / n * 100)) sorted
    sparked = sparkify (map snd (Map.toList m))

runClasses :: Stats -> [Layout ()]
runClasses Stats{ numTests = n, classes } = [ put (intercalate ", " (map (uncurry (class_ n)) (Map.toList classes)) ++ ".") | not (null classes) ] where
  class_ n label n' = if n == n' then label else showFFloatAlt (Just 1) (fromIntegral n' / fromIntegral n * 100 :: Double) ('%':' ':label)

runTables :: Stats -> [Layout ()]
runTables _ = []


plural :: Int -> a -> a -> a
plural 1 s _ = s
plural _ _ p = p

singular :: Int -> String
singular 1 = "s"
singular _ = ""

runTally :: Tally -> [Layout ()]
runTally t =
  [ indentTally $ do
    sepBy_ (put ", " )
      (  [ success (h_ [ put "✓", put (show (successes t)), put (plural (successes t) "success" "successes") ]) | hasSuccesses ]
      ++ [ failure (h_ [ put "✗", put (show (failures t)),  put (plural (failures t)  "failure" "failures") ])  | hasFailures  ])

    put "."
  | hasSuccesses || hasFailures
  ]
  where
  hasSuccesses = successes t /= 0
  hasFailures = failures t /= 0


sepBy_ :: Layout () -> [Layout ()] -> Layout ()
sepBy_ sep = sequence_ . intersperse sep

h_ :: [Layout ()] -> Layout ()
h_ = sepBy_ (put " ")

v_ :: [Layout ()] -> Layout ()
v_ = sepBy_ (line (pure ()))


fromBool :: Bool -> Tally
fromBool = \case
  False -> Tally 0 1
  True  -> Tally 1 0

isFailure :: Tally -> Bool
isFailure = (/= 0) . failures

data Tally = Tally { successes :: Int, failures :: Int }

instance Semigroup Tally where
  Tally s1 f1 <> Tally s2 f2 = Tally (s1 + s2) (f1 + f2)

instance Monoid Tally where
  mempty = Tally 0 0


withSGR :: [SGR] -> Layout a -> Layout a
withSGR sgr m = withHandle $ \ h -> liftIO (hSetSGR h sgr) *> m <* liftIO (hSetSGR h [])

colour :: ColorIntensity -> Color -> Layout a -> Layout a
colour i c = withSGR [SetColor Foreground i c]

success, failure, failure' :: Layout a -> Layout a

success = colour Vivid Green
failure = colour Vivid Red
failure' = colour Dull Red

status :: Maybe Status -> Layout a -> Layout a
status = maybe id (\case { Fail _ -> failure ; Pass -> success })

tropical :: Group
tropical = Group.Group
  { groupName = "Test.Group.Tropical"
  , cases =
    [ semigroupAssoc
    , monoidIdentity
    ]
  }
  where
  semigroupAssoc = Group.Case{ name = "semigroup assoc", loc = here, property = QC.property (\ (ArbTropical a) (ArbTropical b) (ArbTropical c) -> a <> (b <> c) === (a <> b) <> c) }
  monoidIdentity = Group.Case{ name = "monoid identity", loc = here, property = QC.property (\ (ArbTropical a) -> (mempty <> a) === a .&&. (a <> mempty) === a)}

newtype ArbTropical = ArbTropical (Tropical Int)
  deriving (Eq, Ord, Show)

instance QC.Arbitrary ArbTropical where
  arbitrary = QC.oneof
    [ pure (ArbTropical (Tropical Nothing))
    , ArbTropical . Tropical . Just <$> QC.arbitrary
    ]


sparks :: String
sparks = " ▁▂▃▄▅▆▇█"

sparkify :: Real a => [a] -> String
sparkify bins = sparkifyRelativeTo sparks (maximum bins) bins

sparkifyRelativeTo :: Real a => String -> a -> [a] -> String
sparkifyRelativeTo sparks max = fmap spark
  where
  spark n = sparks !! round (realToFrac n / realToFrac max * realToFrac (length sparks - 1) :: Double)


data State = State
  { topStatus   :: Status
  , groupStatus :: Maybe Status
  , caseStatus  :: Maybe Status
  , tally       :: Tally
  }

stat :: a -> (Pos -> a) -> Status -> a
stat pass fail = \case{ Pass -> pass ; Fail pos -> fail pos }

data Status = Pass | Fail Pos

_Fail :: Prism' Status Pos
_Fail = prism' Fail $ \case{ Pass -> Nothing ; Fail p -> Just p }

data Pos = First | Nth

pos :: a -> a -> Pos -> a
pos first nth = \case{ First -> first ; Nth -> nth }

topStatus_ :: Lens' State Status
topStatus_ = lens topStatus (\ s topStatus -> s{ topStatus })

groupStatus_ :: Lens' State (Maybe Status)
groupStatus_ = lens groupStatus (\ s groupStatus -> s{ groupStatus })

caseStatus_ :: Lens' State (Maybe Status)
caseStatus_ = lens caseStatus (\ s caseStatus -> s{ caseStatus })

tally_ :: Lens' State Tally
tally_ = lens tally (\ s tally -> s{ tally })

topIndent :: (Pos -> Layout ()) -> State -> Layout ()
topIndent f = stat space (failure' . f) . topStatus


newtype Layout a = Layout { runLayout :: forall r . (a -> State -> IO r) -> Handle -> State -> IO r }

instance Functor Layout where
  fmap f m = Layout (\ k -> runLayout m (k . f))

instance Applicative Layout where
  pure = liftIO . pure
  Layout f <*> Layout a = Layout $ \ k h -> f (\ f' -> a (\ a' -> k $! f' a') h) h

instance Monad Layout where
  m >>= f = Layout $ \ k h -> runLayout m (\ a -> runLayout (f a) k h) h

instance MonadIO Layout where
  liftIO m = Layout (\ k _ s -> m >>= (`k` s))

tell :: Tally -> Layout ()
tell t = Layout (\ k _ s -> k () $! s & tally_ %~ (<> t))

listen :: Layout a -> Layout (Tally, a)
listen m = Layout $ \ k h s1 -> runLayout m (\ a s2 -> k (tally s2, a) $! s2 & tally_ %~ (tally s1 <>)) h (s1 & tally_ .~ mempty)

withHandle :: (Handle -> Layout a) -> Layout a
withHandle f = Layout $ \ k h -> runLayout (f h) k h


wrap :: (State -> Layout a) -> Layout a
wrap m = Layout $ \ k h s -> runLayout (m s) k h s

heading, line, indentTally :: Layout a -> Layout a

heading m = wrap $ \ s -> do
  case caseStatus s of
    Just (Fail _) -> do
      topIndent (pos heading1 headingN) s
      maybe dvline (failure' . group) (s^?groupStatus_._Just._Fail)
      failure' arrow
    _ -> do
      topIndent (const vline) s
      if is _Just (caseStatus s) then
        dvline *> bullet
      else
        space
  m

line m = wrap (\ s -> do
  topIndent (const vline) s
  when (is _Just (groupStatus s)) dvline
  when (is _Just (caseStatus  s)) (status (caseStatus s) vline)
  m <* nl)

indentTally m = wrap $ \ s -> do
  maybe (topIndent (const end) s) (\case
    Pass   -> topIndent (const vline) s *> space
    Fail _ -> failure' (headingN *> gtally)) (groupStatus s)
  m <* nl

data Side = Top | Bottom

rule :: Side -> Width -> Layout ()
rule side w = wrap $ \ s -> do
  let c = caseStatus s
      h = maybe '┈' (const '─') c
      corner = case side of { Top -> '╭' ; Bottom -> '╰' } : [h]
  topIndent (const vline) s
  when (is _Just c) dvline
  status c (put (corner ++ replicate fullWidth h))
  nl
  where
  fullWidth = width w + 3 + length "Success"


space, bullet, heading1, headingN, arrow, vline, dvline, gtally, end :: Layout ()
space    = put "  "
bullet   = put "☙ "
heading1 = put "╭─"
headingN = put "├─"
arrow    = put "▶ "
vline    = put "│ "
dvline   = put "┊ "
gtally   = put "┤ "
end      = put "╰─┤ "

group :: Pos -> Layout ()
group = put . pos "──" "├─"


nl :: Layout ()
nl = withHandle (liftIO . (`hPutStrLn` ""))

put :: String -> Layout ()
put s = withHandle (liftIO . (`hPutStr` s))

(%=) :: Setter State State a b -> (a -> b) -> Layout ()
o %= f = Layout (\ k _ s -> k () (s & o %~ f))

infix 4 %=

(.=) :: Setter State State a b -> b -> Layout ()
o .= v = o %= const v

(?=) :: Setter State State a (Maybe b) -> b -> Layout ()
o ?= v = o .= Just v
