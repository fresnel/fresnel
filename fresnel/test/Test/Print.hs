{-# LANGUAGE FlexibleContexts #-}
module Test.Print
( withSGR
, success
, failure
, failure'
, sparkify
, topIndent
, withHandle
, line
, section
, space
, bullet
, heading1
, headingN
, arrow
, vline
, hline
, gtally
, end
, nl
, putS
) where

import Control.Effect.Reader
import Control.Effect.State
import Control.Monad (join, when)
import Control.Monad.IO.Class
import Data.Bool (bool)
import Fresnel.Maybe (_Just)
import Fresnel.Optional (is)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), hSetSGR)
import System.IO (Handle, hPutStr, hPutStrLn)
import Test.Group

withSGR :: (Has (Reader Handle) sig m, MonadIO m) => [SGR] -> m a -> m a
withSGR sgr m = withHandle $ \ h -> liftIO (hSetSGR h sgr) *> m <* liftIO (hSetSGR h [])

colour :: (Has (Reader Handle) sig m, MonadIO m) => ColorIntensity -> Color -> m a -> m a
colour i c = withSGR [SetColor Foreground i c]

success, failure, failure' :: (Has (Reader Handle) sig m, MonadIO m) => m a -> m a

success = colour Vivid Green
failure = colour Vivid Red
failure' = colour Dull Red


sparks :: String
sparks = " ▁▂▃▄▅▆▇█"

sparkify :: Real a => [a] -> String
sparkify bins = sparkifyRelativeTo sparks (maximum bins) bins

sparkifyRelativeTo :: Real a => String -> a -> [a] -> String
sparkifyRelativeTo sparks max = fmap spark
  where
  spark n = sparks !! round (realToFrac n / realToFrac max * realToFrac (length sparks - 1) :: Double)


topIndent :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => String -> m ()
topIndent m = gets hasFailures >>= failure' . putS . bool space m

withHandle :: Has (Reader Handle) sig m => (Handle -> m a) -> m a
withHandle = join . asks


line :: (Has (Reader Handle) sig m, Has (State Tally) sig m, MonadIO m) => m ()
line = topIndent vline *> putS vline

section :: (Has (Reader Handle) sig m, Has (Reader Width) sig m, Has (State Tally) sig m, MonadIO m) => Maybe Status -> m a -> m a
section s m = do
  fullWidth <- asks ((+ (3 + length "Success")) . width)
  let rule corner = indent *> maybe id (stat success failure) s (putS (corner : h : replicate fullWidth h)) *> nl
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
