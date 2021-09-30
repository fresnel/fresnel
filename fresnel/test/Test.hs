{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main
( main
) where

import           Control.Applicative (liftA2)
import           Control.Monad (guard, join, when, (<=<))
import           Control.Monad.IO.Class
import           Data.Bool (bool)
import           Data.Foldable (fold, for_, toList, traverse_)
import           Data.Function ((&))
import qualified Data.IntMap as IntMap
import           Data.List (elemIndex, intercalate, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup (stimes)
import qualified Fold.Test
import           Fresnel.Fold (preview)
import           Fresnel.Getter (Getter, to, (^.))
import           Fresnel.Lens (Lens', lens)
import           Fresnel.Maybe (_Just)
import           Fresnel.Optional (is)
import           Fresnel.Prism (Prism', prism')
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

run :: [Group] -> Options -> IO Bool
run groups (Options gs _ args) = not . isFailure . tally <$> runLayout (do
  (t, _) <- listen (traverse_ (runGroup args w) (matching ((==) . groupName) gs groups))
  sequence_ (runTally t)) (const pure) stdout (State Nothing mempty)
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

runGroup :: MonadIO m => Args -> Width -> Group -> Layout m ()
runGroup args width Group.Group{ groupName, cases } = do
  bookend groupState_ (mempty, Nothing) $ do
    heading First $ withSGR [SetConsoleIntensity BoldIntensity] $ put groupName *> nl
    sandwich True width' (sequence_ (intersperse blank (map (bookend caseStatus_ Pass . fmap unit . runCase args width) cases)))
    t <- use (groupState_.to (fmap fst))
    maybe (pure mempty) (sequence_ . runTally) t
  blank
  where
  width' = width <> stimes (2 :: Int) one

bookend :: Setter State State a (Maybe b) -> b -> Layout m c -> Layout m c
bookend o v m = o ?= v *> m <* o .= Nothing

sandwich :: MonadIO m => Bool -> Width -> Layout m a -> Layout m a
sandwich cond w m = when cond (rule Top w) *> m <* when cond (rule Bottom w)

runCase :: MonadIO m => Args -> Width -> Case -> Layout m Status
runCase args w Group.Case{ name, loc = Loc{ path, lineNumber }, property } = do
  title First False

  pos <- use (tally_.to (bool First Nth . isFailure))
  res <- liftIO (quickCheckWithResult args property)
  let stat' = if isSuccess res then Pass else Fail
  caseStatus_ ?= stat'
  tally_ %= (<> unit stat')

  withHandle (\ h -> do
    isTerminal <- liftIO (hIsTerminalDevice h)
    when (isTerminal && not (isSuccess res)) $ do
      liftIO (hClearFromCursorToLineBeginning h)
      liftIO (hSetCursorColumn h 0)
      failure (title pos True))

  put "   " *> stat (success (put "Success")) (failure (put "Failure")) stat' *> nl

  let stats = resultStats res
      details = numTests stats == maxSuccess args && not (null (classes stats))
      labels = runLabels stats

  sandwich (details || not (isSuccess res) || not (null labels)) w $ v_ $ concat
    [ [ line (h_ (runStats args stats ++ runClasses stats)) | details ]
    , do
      Failure{ usedSeed, usedSize, reason, theException, failingTestCase } <- pure res
      pure (do
        line (put (path ++ ":" ++ show lineNumber))
        line (put reason)
        for_ theException (line . put . displayException)
        for_ failingTestCase (line . put))
        <> [ line (put ("--replay '(" ++ show usedSeed ++ "," ++ show usedSize ++ ")'")) ]
    , labels
    , runTables stats
    ]
  pure stat'
  where
  title pos failed = heading pos $ do
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

runStats :: MonadIO m => Args -> Stats -> [Layout m ()]
runStats Args{ maxSuccess } Stats{ numTests, numDiscarded, numShrinks } = [ sepBy_ (put ", ") entries *> put "." | not (null entries) ]
  where
  entries = concat
    [ [ put (show numTests ++ " test" ++ singular numTests) | numTests > 0 && numTests /= maxSuccess ]
    , [ put (show numDiscarded ++ " discarded") | numDiscarded > 0 ]
    , [ put (show numShrinks ++ " shrink" ++ singular numShrinks) | numShrinks > 0 ]
    ]


runLabels :: MonadIO m => Stats -> [Layout m ()]
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

runClasses :: MonadIO m => Stats -> [Layout m ()]
runClasses Stats{ numTests = n, classes } = [ put (intercalate ", " (map (uncurry (class_ n)) (Map.toList classes)) ++ ".") | not (null classes) ] where
  class_ n label n' = if n == n' then label else showFFloatAlt (Just 1) (fromIntegral n' / fromIntegral n * 100 :: Double) ('%':' ':label)

runTables :: Stats -> [Layout m ()]
runTables _ = []


plural :: Int -> a -> a -> a
plural 1 s _ = s
plural _ _ p = p

singular :: Int -> String
singular 1 = "s"
singular _ = ""

runTally :: MonadIO m => Tally -> [Layout m ()]
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


sepBy_ :: Monoid a => Layout m a -> [Layout m a] -> Layout m a
sepBy_ sep = fold . intersperse sep

h_ :: MonadIO m => [Layout m ()] -> Layout m ()
h_ = sepBy_ (put " ")

v_ :: MonadIO m => [Layout m ()] -> Layout m ()
v_ = sepBy_ blank


isFailure :: Tally -> Bool
isFailure = (/= 0) . failures

unit :: Status -> Tally
unit = stat (Tally 1 0) (Tally 0 1)

data Tally = Tally { successes :: Int, failures :: Int }

instance Semigroup Tally where
  Tally s1 f1 <> Tally s2 f2 = Tally (s1 + s2) (f1 + f2)

instance Monoid Tally where
  mempty = Tally 0 0


withSGR :: MonadIO m => [SGR] -> Layout m a -> Layout m a
withSGR sgr m = withHandle $ \ h -> liftIO (hSetSGR h sgr) *> m <* liftIO (hSetSGR h [])

colour :: MonadIO m => ColorIntensity -> Color -> Layout m a -> Layout m a
colour i c = withSGR [SetColor Foreground i c]

success, failure, failure' :: MonadIO m => Layout m a -> Layout m a

success = colour Vivid Green
failure = colour Vivid Red
failure' = colour Dull Red

status :: MonadIO m => Maybe Status -> Layout m a -> Layout m a
status = maybe id (stat success failure)

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
  { groupState :: Maybe (Tally, Maybe Status)
  , tally      :: Tally
  }

stat :: a -> a -> Status -> a
stat pass fail = \case{ Pass -> pass ; Fail -> fail }

data Status = Pass | Fail

_Fail :: Prism' Status ()
_Fail = prism' (const Fail) $ \case{ Pass -> Nothing ; Fail -> Just () }

data Pos = First | Nth

groupState_ :: Lens' State (Maybe (Tally, Maybe Status))
groupState_ = lens groupState (\ s groupState -> s{ groupState })

caseStatus_ :: Lens' State (Maybe Status)
caseStatus_ = lens (snd <=< groupState) (\ s st -> s{ groupState = Just (maybe id ((<>) . unit) st (maybe mempty fst (groupState s)), st) })

tally_ :: Lens' State Tally
tally_ = lens tally (\ s tally -> s{ tally })

topIndent :: MonadIO m => Layout m () -> Bool -> Layout m ()
topIndent = bool (put space) . failure'

isInGroup :: State -> Bool
isInGroup = is (groupState_._Just)

isInCase :: State -> Bool
isInCase = is (caseStatus_._Just)

isInFailCase :: State -> Bool
isInFailCase = is (caseStatus_._Just._Fail)

caseStatus :: State -> Maybe Status
caseStatus = join . preview caseStatus_


newtype Layout m a = Layout { runLayout :: forall r . (a -> State -> m r) -> Handle -> State -> m r }

instance Semigroup a => Semigroup (Layout m a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Layout m a) where
  mempty = pure mempty

instance Functor (Layout m) where
  fmap f m = Layout (\ k -> runLayout m (k . f))

instance Applicative (Layout m) where
  pure a = Layout $ \ k _ -> k a
  Layout f <*> Layout a = Layout $ \ k h -> f (\ f' -> a (\ a' -> k $! f' a') h) h

instance Monad (Layout m) where
  m >>= f = Layout $ \ k h -> runLayout m (\ a -> runLayout (f a) k h) h

instance MonadIO m => MonadIO (Layout m) where
  liftIO m = Layout (\ k _ s -> liftIO m >>= (`k` s))

listen :: Layout m a -> Layout m (Tally, a)
listen m = Layout $ \ k h s1 -> runLayout m (\ a s2 -> k (tally s2, a) $! s2 & tally_ %~ (tally s1 <>)) h (s1 & tally_ .~ mempty)

withHandle :: (Handle -> Layout m a) -> Layout m a
withHandle f = Layout $ \ k h -> runLayout (f h) k h


wrap :: (State -> Layout m a) -> Layout m a
wrap m = Layout $ \ k h s -> runLayout (m s) k h s

blank :: (MonadIO m, Monoid a) => Layout m a
blank = line (pure mempty)

heading :: MonadIO m => Pos -> Layout m a -> Layout m a

heading p m = wrap $ \ s -> do
  if isInFailCase s then do
    topIndent (headingGutter p) (isFailure (tally s))
    failure' (group First *> put arrow)
  else do
    topIndent (put vline) (isFailure (tally s))
    put $ if isInCase s then
      vline <> bullet
    else
      space
  m

line, indentTally :: MonadIO m => Layout m a -> Layout m a

line m = wrap (\ s -> do
  topIndent (put vline) (isFailure (tally s))
  when (isInGroup s) $ do
    put vline
    when (isInCase s) (status (caseStatus s) (put vline))
  m <* nl)

indentTally m = wrap $ \ s -> do
  case groupState s of
    Nothing         -> topIndent (put end) (isFailure (tally s))
    Just (t, _)
      | isFailure t -> failure' (put (headingN <> gtally))
      | otherwise   -> topIndent (put vline) (isFailure (tally s)) *> put space
  m <* nl

data Side = Top | Bottom

rule :: MonadIO m => Side -> Width -> Layout m ()
rule side w = wrap $ \ s -> do
  let c = caseStatus s
      corner = case side of { Top -> '╭' ; Bottom -> '╰' } : [h]
  topIndent (put vline) (isFailure (tally s))
  when (isInCase s) (put vline)
  status c (put (corner ++ replicate fullWidth h))
  nl
  where
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

group :: MonadIO m => Pos -> Layout m ()
group = put . \case
  First -> hline
  Nth   -> vlineR

headingGutter :: MonadIO m => Pos -> Layout m ()
headingGutter = put . \case
  First -> heading1
  Nth   -> headingN


nl :: MonadIO m => Layout m ()
nl = withHandle (liftIO . (`hPutStrLn` ""))

put :: MonadIO m => String -> Layout m ()
put s = withHandle (liftIO . (`hPutStr` s))

(%=) :: Setter State State a b -> (a -> b) -> Layout m ()
o %= f = Layout (\ k _ s -> k () (s & o %~ f))

infix 4 %=

(.=) :: Setter State State a b -> b -> Layout m ()
o .= v = o %= const v

(?=) :: Setter State State a (Maybe b) -> b -> Layout m ()
o ?= v = o .= Just v

use :: Getter State a -> Layout m a
use o = Layout (\ k _ s -> k (s^.o) s)
